(library (sph web app)
  (export
    branch-ref
    branch-route-path
    call-hook
    default-server-error-handler
    import-branches
    match-path
    scgi-headers-get-content-length
    shtml-includes-proc
    sph-web-app-description
    swa-server-http
    swa-server-scgi
    swa-start)
  (import
    (sph)
    (sph web app start)
    ;(sph conditional)
    ;(sph documentation)
    ;(sph hashtable)
    ;(sph web http)
    ;(sph web shtml)
    ;(only (sph list) any->list)
    ;(only (sph string) string-quote)
    )

  (define sph-web-app-description
    "main module that exports swa-start, swa-server-scgi and various helpers")

  ;-- server

  (define-syntax-rule (start-message address port)
    (display
      (string-append "listening on " address
        (if (and (not (string-prefix? "/" address)) port) (string-append ":" (number->string port))
          "")
        "\n")))

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

  (define (scgi-headers-get-content-length h)
    ; get the request-body-size from headers
    (alist-ref h "CONTENT_LENGTH"))

  (define (default-server-error-handler obj resume socket) (log-message (q error) obj)
    (if (port-closed? socket) #f (resume)))

  (define*
    (swa-server-scgi swa-env swa-app #:key (http-respond swa-http-respond)
      (exception-handler default-server-error-handler)
      (exception-keys #t))
    "vector procedure:{headers client app-respond} false/procedure:{key resume exception-arguments ...} boolean/(symbol ...) ->
     starts a server listens for scgi requests and calls app-respond for each new request.
     currently the given exception handlers are only installed when the \"development\" config option is unset or false.
     swa-scgi uses (sph scgi) which uses (sph server)"
    (debug-log swa-env swa-app)
    #;(let
      ( (listen-address
          (or (config-ref listen-address)
            (if-pass (config-ref socket-name)
              (l (a) (string-append (dirname scgi-default-address) "/" a)) scgi-default-address)))
        (listen-port (config-ref listen-port)))
      (call-with-socket listen-address listen-port
        (l (socket) (start-message listen-address listen-port)
          (if (or (config-ref single-threaded) (config-ref development))
            ;single-threaded without extra exception handler
            (scgi-handle-requests (l (headers client) (http-respond headers client app-respond))
              socket 1)
            ;possibly multi-threaded and all exceptions catched for continuous processing
            (scgi-handle-requests (l (headers client) (http-respond headers client app-respond))
              socket (config-ref workers) #f #f exception-handler exception-keys))
          (display "stopped listening. ")))))

  (define (swa-server-http swa-env swa-app)
    "list string/rnrs-hashtable false/procedure:{key resume exception-arguments ...} boolean/ (symbol ...) ->
     starts a server to process http requests. no scgi proxy needed.
     this uses guiles (web server) and has not been tested much yet"
    (apply
      (l (listen-address listen-port app-init app-respond app-deinit) (app-init)
        (call-with-socket listen-address listen-port
          (l (socket) (start-message listen-address listen-port)
            (run-server
              (l (request request-body)
                (let*
                  ( (headers
                      (pair (pair "request_uri" (uri->string (request-uri request)))
                        (map (l (a) (pair (symbol->string (first a)) (tail a)))
                          (request-headers request))))
                    (response (app-respond (alist-ref headers "request_uri") headers #f)))
                  (values
                    (read-headers
                      (open-input-string
                        (string-append (apply string-append (swa-http-response-header response))
                          "\r\n")))
                    (swa-http-response-body response))))
              (q http) (list #:socket socket))))
        (app-deinit))
      (or (config-ref listen-address) "::1") (or (config-ref listen-port) 8080) swa-app))

  ;-- other

  (define-syntax-rule (match-path path specs ...)
    ; split a path into parts and use ice-9 match to match parts
    (match (tail (path->list path)) specs ...))

  (define-syntax-case (import-branches) s
    ; imports all modules on the first level in the branch/ directory.
    (let (module-names (map first (module-find (string-append swa-root "branch/") #:max-depth 1)))
      (datum->syntax s (pair (q import) module-names))))

  (define-syntax-rule
    (branch-ref unquoted-branch-name unquoted-binding-name unquoted-project-name ...)
    ; get a binding from a branch at run-time. with this, branch modules do not need to depend on each other
    ; to make internal redirects
    (primitive-branch-ref (q unquoted-branch-name) (q unquoted-binding-name)
      (q unquoted-project-name) ...))

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

  (define (shtml-includes-proc sources-css sources-javascript) "usage: (shtml-include (q css) ref)"
    (let-syntax
      ( (get-sxml
          (syntax-rule (format ref sources client-file create-include-sxml)
            (map create-include-sxml
              (append (if-pass (apply client-file #f sources) any->list (list))
                (if-pass (ref format #f) any->list (list)))))))
      (l (format ref)
        (if (equal? (q css) format)
          (get-sxml format ref sources-css client-css-file shtml-include-css)
          (get-sxml format ref sources-javascript client-javascript-file shtml-include-javascript)))))

  (define (call-hook hook-procedures hook-name . a)
    "rnrs-hashtable symbol procedure-arguments ... -> any
     if a procedure with hook-name exists in hook-procedures, then it is applied with \"a\"
     if not, \"a\" is the result"
    (if hook-procedures
      (let (proc (hashtable-ref hook-procedures hook-name)) (if proc (apply proc a) a)) a)))
