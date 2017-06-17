(library (sph web app)
  (export
    branch-ref
    branch-route-path
    client-css
    client-css-file
    client-file
    client-html
    client-html-file
    client-javascript
    client-javascript-file
    client-port
    config-ref
    config-set!
    headers-content-length
    import-all-branches
    log-message
    match-path
    primitive-branch-ref
    respond
    respond-html
    respond-type
    swa-http-response
    swa-http-response*
    swa-initialise-config
    swa-module-prefix
    swa-paths
    swa-project-name
    swa-request
    swa-root
    swa-search-load-paths
    swa-start
    swa-start-http
    swa-start-scgi)
  (import
    (guile)
    (ice-9 match)
    (ice-9 threads)
    (rnrs bytevectors)
    (sph base)
    (sph conditional)
    (sph config)
    (sph log)
    (sph module)
    (sph scgi)
    (sph web app base)
    (sph web app client)
    (sph web app http)
    (sph web http)
    (web http)
    (web request)
    (web server)
    (web uri)
    (only (sph process) shell-eval)
    (only (sph server) server-create-bound-socket))

  (define-syntax-rule (swa-path->module-name a)
    (let (path (swa-search-load-paths a))
      (if path
        (path->symbol-list
          (string-drop path (string-length (string-longest-prefix path %load-path))))
        path)))

  (define-syntax-rule (swa-module-name->path a)
    (%search-load-path (string-append (string-join (map symbol->string a) "/") ".scm")))

  (define-syntax-rule (swa-module-name->root-path a)
    (string-longest-prefix (swa-module-name->path a) swa-paths))

  (define (swa-path->root-path a) "string -> string/boolean" (string-longest-prefix a swa-paths))

  (define primitive-branch-ref
    (procedure->cached-procedure
      (l* (branch-name binding-name #:optional project-name)
        "symbol symbol symbol/(symbol ...) -> procedure/boolean:false
        the binding name is constructed as {branch-name}-{binding-name}.
        if project-name is not given, the binding is searched in all swa-paths"
        (let
          (module-name
            (if (symbol? project-name) (pair project-name (list (q branch) branch-name))
              (if (list? project-name) (append project-name (list (q branch) branch-name))
                (swa-path->module-name
                  (string-append "branch/" (symbol->string branch-name) ".scm")))))
          (and-let*
            ( (module (resolve-module module-name #:ensure #f))
              (variable (module-variable module (symbol-append branch-name (q -) binding-name))))
            (variable-ref variable))))))

  (define (branch-route-path project-name path . arguments)
    "false/symbol/(symbol ...) string any ... -> any
     select the branch procedure to apply from the path of the url.
     branch-name[/binding-name-part ...]
     binding-name-parts are joined with \"-\"
     for example, the input path \"content/view\" would try to call the procedure content-view from the module (project-name branch content) with the given arguments.
     project-name can be false, in which case the procedure is searched in all imported modules until one is found."
    (apply
      (l (branch . variable)
        (if (null? variable) (respond 404)
          (let
            (proc
              (primitive-branch-ref (string->symbol branch)
                (string->symbol (string-join variable "-")) project-name))
            (if proc (apply proc arguments) (respond 404)))))
      (tail (string-split path #\/))))

  (define-syntax-rules respond-type
    ((type-key body) (respond 200 (list (swa-http-header-content-type type-key)) body))
    ( (type-key status header-lines body)
      (respond status (pair (swa-http-header-content-type type-key) header-lines) body)))

  (define-syntax-rule (headers-content-length h)
    ;get the request-body-size from the scgi-headers
    (alist-ref h "CONTENT_LENGTH"))

  (define-syntax-rule (respond args ...) (swa-http-response args ...))

  (define* (respond-html bindings source #:optional (headers (list)))
    (respond 200 (pair "content-type:text/html\r\n" headers)
      (l (client) (client-html client bindings source))))

  (define-syntax-rule
    (branch-ref unquoted-branch-name unquoted-binding-name unquoted-project-name ...)
    ;get a binding from a branch at run-time. with this, branch modules do not need to depend on each other
    ;to make internal redirects
    (primitive-branch-ref (q unquoted-branch-name) (q unquoted-binding-name)
      (q unquoted-project-name) ...))

  (define (import-path->swa-path a)
    (any
      (l (e)
        (let (full-path (string-append e "/" a "/")) (if (file-exists? full-path) full-path #f)))
      %load-path))

  (define (swa-sync-import-root-files swa-root . paths)
    "symlink non-generated files from the imports root-directories into the current root.
     currently requires that the filesystem supports symlinks. symlinks are also not deleted if the files are removed"
    (each
      (l (a)
        (shell-eval
          (string-append
            "cp --recursive --no-clobber --dereference --symbolic-link --target-directory="
            (string-append swa-root "root") " " (string-append a "root/*") " 2> /dev/null")))
      paths))

  (define-syntax-rule (match-path path specs ...) (match (tail (path->list path)) specs ...))

  (define-syntax-case (import-all-branches) s
    ;"imports all modules in the branch/ directory. not recursively, only the modules on the first level.
    ;a catch here is to load the branch module files relative to the currently evaluated file.
    ;(current-filename) gave #f"
    (let*
      ( (swa-root (swa-module-name->root-path (module-name (current-module))))
        (library-names (module-find (string-append swa-root "branch/") #:max-depth 1)))
      (datum->syntax s (pair (q import) library-names))))

  (define-syntax-rule (import-main module-prefix)
    ;import the main.scm module
    (module-use! (current-module) (resolve-interface (append module-prefix (q (main))))))

  (define-syntax-rule (start-message address port)
    (display
      (string-append "listening on " address
        (if (and (not (string-prefix? "/" address)) port) (string-append ":" (number->string port))
          "")
        "\n")))

  (define-syntax-rule (swa-initialise-module-prefix swa-root)
    (let*
      ( (swa-root (string-trim-right swa-root #\/)) (load-path (path->load-path swa-root))
        (relative-path
          (if load-path (string-drop-prefix (string-append (realpath* load-path) "/") swa-root)
            (basename swa-root)))
        (prefix (map string->symbol (string-split relative-path #\/))))
      (if (not load-path) (add-to-load-path (dirname swa-root))) (set! swa-module-prefix prefix)
      (set! swa-project-name (basename relative-path)) (import-main prefix)))

  "web-projects/harkona/sph-cms"
  "web-projects-harkona-sph-cms"

  (define (swa-initialise-config config) (config-load swa-default-config)
    (guard (obj ((if config #f (eq? (first obj) (q configuration-file-does-not-exist))) #f))
      (config-load config)))

  (define (default-server-error-handler obj resume socket) (log-message (q error) obj)
    (if (port-closed? socket) #f (resume)))

  (define-syntax-rule (local-socket-set-options address)
    (if (string-prefix? "/" address)
      ; set socket permissions and group if configured in config file
      (let ((perm (config-ref socket-permissions)) (group (config-ref socket-group)))
        (if (integer? perm) (chmod address perm))
        ; socket group setting can be tricky: in some circumstances or on some platforms it is not possible
        (if group (chown address -1 (if (string? group) (group:gid (getgrnam group)) group))))))

  (define (call-with-socket listen-address listen-port proc)
    (let (socket (server-create-bound-socket listen-address listen-port))
      (local-socket-set-options listen-address) (proc socket)))

  (define*
    (swa-start-scgi #:optional (imports (list)) config #:key (http-respond swa-http-respond)
      (exception-handler default-server-error-handler)
      (exception-keys #t))
    "list string/rnrs-hashtable false/procedure:{key resume exception-arguments ...} boolean/(symbol ...) ->
     starts a server to process scgi requests.
     this uses (sph scgi) which uses (sph server).
     currently the given exception handlers are only installed when the \"development\" config option is unset or false"
    (swa-start
      (l (app-respond)
        (let
          ( (listen-address
              (or (config-ref listen-address)
                (if-pass (config-ref socket-name)
                  (l (a) (string-append (dirname scgi-default-address) "/" a)) scgi-default-address)))
            (listen-port (config-ref listen-port)))
          (call-with-socket listen-address listen-port
            (l (socket) (start-message listen-address listen-port)
              (if (or (config-ref single-threaded) (config-ref development))
                ;single-threaded without extra exception handler
                (scgi-handle-requests
                  (l (headers client) (http-respond headers client app-respond)) socket 1)
                ;possibly multi-threaded and all exceptions catched for continuous processing
                (scgi-handle-requests
                  (l (headers client) (http-respond headers client app-respond)) socket
                  (config-ref workers) #f #f exception-handler exception-keys))
              (display "stopped listening. ")))))
      imports config))

  (define* (swa-start-http #:optional (imports (list)) config)
    "list string/rnrs-hashtable false/procedure:{key resume exception-arguments ...} boolean/ (symbol ...) ->
     starts a server to process http requests. no scgi proxy needed.
     this uses guiles (web server) and has not been tested much yet"
    (swa-start
      (l (app-respond)
        (let
          ( (listen-address (or (config-ref listen-address) "::1"))
            (listen-port (or (config-ref listen-port) 8080)))
          (call-with-socket listen-address listen-port
            (l (socket) (start-message listen-address listen-port)
              (run-server
                (l (request request-body)
                  (let*
                    ( (headers
                        (pair (pair "request_uri" (uri->string (request-uri request)))
                          (map (l (e) (pair (symbol->string (first e)) (tail e)))
                            (request-headers request))))
                      (response (app-respond (alist-ref headers "request_uri") headers #f)))
                    (values
                      (read-headers
                        (open-input-string
                          (string-append (apply string-append (swa-http-response-header response))
                            "\r\n")))
                      (swa-http-response-body response))))
                (q http) (list #:socket socket)))))
        (display "stopped listening. "))
      imports config))

  (define* (swa-start proc #:optional (imports (list)) config)
    "(string:guile-load-path-relative-path ...) string/rnrs-hashtable ->
     procedure:{procedure:{headers client ->}:respond ->} procedure ->
     initialises the application, calls proc, and afterwards deinitialises the application.
     must be called with the project directory as current working directory.
     paths in imports should be directing to other web-app projects, relative
     to the load-path that is used by guile. these projects being \"imported\"
     means they will be recognised in procedures like \"branch-ref\" and \"client-template\".
     asset compilation results are always stored in the current project, even if the source files are from imported projects.
     all other files from the \"root/\" of imported-projects are symlinked into the parent \"root/\".
     calls app-initialise and app-deinitialise without arguments if they are defined"
    (set! swa-root (string-append (getcwd) "/"))
    (set! swa-paths (pair swa-root (map import-path->swa-path imports)))
    (apply swa-sync-import-root-files swa-paths) (swa-initialise-module-prefix swa-root)
    (swa-initialise-config config)
    (let* ((m (current-module)) (app-respond (module-ref m (q app-respond))))
      (call-if-defined m (q app-initialise)) (proc app-respond)
      (call-if-defined m (q app-deinitialise)))))
