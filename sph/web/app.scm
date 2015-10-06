(library (sph web app)
  (export
    branch-ref
    branch-route
    client-html
    client-lang->env
    client-script
    client-style
    client-templates
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
    swa-init-config
    swa-library-prefix
    swa-paths
    swa-project-name
    swa-request
    swa-root
    swa-start
    swa-start-http
    swa-start-scgi
    symbol-hashtable)
  (import
    (guile)
    (ice-9 match)
    (ice-9 threads)
    (rnrs base)
    (rnrs bytevectors)
    (sph)
    (sph conditional)
    (sph config)
    (sph hashtable)
    (sph log)
    (sph scgi)
    (sph web app client)
    (sph web app config)
    (sph web app http)
    (sph web http)
    (web http)
    (web request)
    (web server)
    (web uri)
    (only (sph alist) alist-ref)
    (only (sph filesystem) path->list path->full-path)
    (only (sph module)
      module-ref-no-error
      call-if-defined
      path->module-name
      path->symbol-list
      directory-tree-module-names)
    (only (sph one) procedure->cached-procedure program-path)
    (only (sph server) server-create-bound-socket)
    (only (sph string) string-longest-prefix)
    (only (srfi srfi-1) last))

  (define-syntax-rule (swa-search-load-paths a)
    (any (l (ele) (let (path (string-append ele a)) (if (file-exists? path) path #f))) swa-paths))

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

  (define-syntax-rule (branch-route project-name path args ...)
    ;"select the branch-procedure to apply from the path of the url.
    ;branch-name[/binding-name-part ...]
    ;binding-name-parts are joined with \"-\"
    ;example
    ;\"content/view\" maps to the binding content-view in the module (project-name branch content).
    ;the project-name argument limits the imported/extended projects to search in which might decrease execution time."
    (apply
      (l (branch . variable)
        (if (null? variable) (respond 404)
          (let
            (proc
              (primitive-branch-ref (string->symbol branch)
                (string->symbol (string-join variable "-")) project-name))
            (if proc (proc args ...) (respond 404)))))
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
    ;get a binding from a branch at run-time. with this branch modules do not need to depend on each other
    ;to make internal redirects
    (primitive-branch-ref (q unquoted-branch-name) (q unquoted-binding-name)
      (q unquoted-project-name) ...))

  (define (import-path->swa-path a)
    (any
      (l (ele)
        (let (full-path (string-append ele "/" a "/")) (if (file-exists? full-path) full-path #f)))
      %load-path))

  (define (swa-sync-import-root-files swa-root . paths)
    "copy non-generated files from the imports root-directories into the current root.
    currently requires that the filesystem supports symlinks"
    (each
      (l (ele)
        (system
          (string-join
            (list "cp" "-Lrsut" (string-append swa-root "root") (string-append ele "root/*")) " ")))
      paths))

  (define-syntax-rule (match-path path specs ...) (match (tail (path->list path)) specs ...))

  (define-syntax-rule (import-all-branches)
    ;this is syntax to make (current-module) return the local module where
    ;import-all-branches is applied instead of the toplevel module.
    ;swa-start sets "swa-paths", if a file containing it is evaluated directly wihout a swa-start call compilation would fail
    (if swa-paths
      (let (m (current-module))
        (map (compose (l (e) (module-use! m e)) resolve-interface)
          (directory-tree-module-names
            (string-append (swa-module-name->root-path (module-name m)) "branch/") 1)))))

  (define-syntax-rule (import-init library-prefix)
    ;import the init.scm module
    (module-use! (current-module) (resolve-interface (append library-prefix (q (init))))))

  (define-syntax-rule (start-message address port)
    (display
      (string-append "listening on " address
        (if (and (not (string-prefix? "/" address)) port) (string-append ":" (number->string port))
          "")
        "\n")))

  (define-syntax-rule (swa-init-library-prefix swa-root)
    ;todo: check for malfunction because of different path prefixes in swa-paths to same destination directories because of symlink resolution
    (let*
      ( (library-prefix (path->module-name swa-root))
        (library-prefix
          (if library-prefix library-prefix
            (begin (add-to-load-path (dirname swa-root)) (path->module-name swa-root)))))
      (if library-prefix
        (begin (set! swa-library-prefix library-prefix)
          (set! swa-project-name (symbol->string (last library-prefix))) (import-init library-prefix))
        (throw (q project-not-in-load-path) "this is required for loading application parts"
          (q search-paths) %load-path (q guessed-root-directory) swa-root))))

  (define (swa-init-config config) (config-load swa-default-config)
    (catch (if config (q none) (q configuration-file-does-not-exist)) (l () (config-load config))
      (l args #f)))

  (define (default-server-error-handler key resume . args) (log-message (q error) (pair key args))
    (resume))

  (define-syntax-rule (local-socket-set-options address)
    (if (string-prefix? "/" address)
      (let ((perm (config-ref socket-permissions)) (group (config-ref socket-group)))
        (if (integer? perm) (chmod address perm))
        ;socket group setting can be tricky - under some circumstances or on some platforms it is not possible
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
                (pass-if (config-ref socket-name)
                  (l (a) (string-append (dirname scgi-default-address) "/" a)) scgi-default-address)))
            (listen-port (config-ref listen-port)))
          (call-with-socket listen-address listen-port
            (l (socket) (start-message listen-address listen-port)
              (if (config-ref development)
                ;single-threaded without exception handler
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
    starts a server to process http requests.
    this uses guiles (web server) and has been implemented more recently"
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
    initialises the application, calls proc, and deinitialises the application.
    the paths in imports should be directing to other web-app projects relative
    to the load-path that is used by guile. these projects being \"imported\"
    means they will be recognised in procedures like \"branch-ref\" and \"client-template\".
    it is no problem to use (sph lang template) source-files in the parent project, the compilation
    results are always stored in the parents \"root/\" directory.
    all other files from the \"root/\" of imported-projects will be symlinked into the parent \"root/\".
    applies app-init without arguments if it is defined in the current-module"
    (set! swa-root (string-append (getcwd) "/"))
    (set! swa-paths (pair swa-root (map import-path->swa-path imports)))
    (apply swa-sync-import-root-files swa-paths) (swa-init-library-prefix swa-root)
    (swa-init-config config) (client-init)
    (let* ((m (current-module)) (app-respond (module-ref m (q app-respond))))
      (call-if-defined m (q app-init)) (proc app-respond)
      (call-if-defined m (q app-exit)) (display "exit.\n"))))