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
    import-branches
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

  (define (swa-path->root-path a) "string -> string/boolean" (string-longest-prefix a swa-paths))

  (define (import-path->swa-path a)
    (any
      (l (e)
        (let (full-path (string-append e "/" a "/")) (if (file-exists? full-path) full-path #f)))
      %load-path))

  (define-syntax-rule (import-main module-prefix)
    ; (symbol ...) ->
    ; import the {swa-root}/main.scm module
    (module-use! (current-module) (resolve-interface (append module-prefix (q (main))))))

  (define-syntax-rule (swa-initialise-module-prefix swa-root)
    (let*
      ( (swa-root (string-trim-right swa-root #\/)) (load-path (path->load-path swa-root))
        (relative-path
          (if load-path (string-drop-prefix (string-append (realpath* load-path) "/") swa-root)
            (basename swa-root)))
        (prefix (map string->symbol (string-split relative-path #\/))))
      (if (not load-path) (add-to-load-path (dirname swa-root))) (set! swa-module-prefix prefix)
      (set! swa-project-name (basename relative-path)) (import-main prefix)))

  (define (swa-initialise-config config) (config-load swa-default-config)
    (guard (obj ((if config #f (eq? (first obj) (q configuration-file-does-not-exist))) #f))
      (config-load config)))

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

  (define (swa-link-root-files swa-root . paths)
    "symlink non-generated files from the imports root-directories into the current root.
     currently requires that the filesystem supports symlinks. symlinks are also not deleted if the files are removed"
    (each
      (l (a)
        (shell-eval
          (string-append
            "cp --recursive --no-clobber --dereference --symbolic-link --target-directory="
            (string-append swa-root "root") " " (string-append a "root/*") " 2> /dev/null")))
      paths))

  (define-syntax-rule (swa-config-get swa-root name) "string string -> list"
    (let
      ( (path (string-append swa-root "/config/" name ".scm"))
        (tree-map-lists-and-self (compose alist->hashtable list->alist)
          (primitive-eval (list (q quasiquote) (file->datums path)))))))

  (define-syntax-rule (swa-paths-get projects)
    "((symbol ...) ...) -> (string ...)
     convert project names to full paths.
     the current project is included"
    (map
      (l (a)
        (let (relative-path (string-join (map symbol->string a) "/"))
          (any
            (l (load-path)
              (let (path (string-append load-path "/" relative-path))
                (and (file-exists? path) load-path)))
            %load-path)))
      projects))

  (define-syntax-rule (swa-import-main name) (qq (import ((unquote-splicing name) main))))

  (define-syntax-case (swa-init projects config-name) (swa-import-main (first projects))
    (define swa-paths (swa-paths-get projects)) (define swa-root (first swa-paths))
    (define swa-config (swa-config-get (first swa-imports) config-name))
    (apply swa-link-root-files swa-paths)
    ;(app-init)
    )

  (define (swa-deinit) (app-exit))

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
