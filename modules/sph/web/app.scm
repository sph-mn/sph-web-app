(library (sph web app)
  (export
    default-server-error-handler
    match-path
    scgi-headers-get-content-length
    sph-web-app-description
    swa-config-ref
    swa-env
    swa-env-config
    swa-env-paths
    swa-env-root
    swa-server-http
    swa-server-scgi
    swa-start)
  (import
    (sph base)
    (sph log)
    (sph server)
    (sph web app http)
    (sph web app start)
    (web http)
    (web request)
    (web server)
    (web uri)
    (only (guile) port-closed?))

  (define sph-web-app-description
    "main module that exports swa-start, swa-server-scgi and various helpers")

  (define-syntax-rule (swa-config-ref swa-env key)
    ; get the value for an unquoted config key from a swa-env object.
    ; (swa-config-ref swa-env testkey)
    (hashtable-ref (swa-env-config swa-env) (quote key)))

  ;-- server

  (define-syntax-rule (start-message address port)
    (display
      (string-append "listening on " address
        (if (and (not (string-prefix? "/" address)) port) (string-append ":" (number->string port))
          "")
        "\n")))

  (define (local-socket-set-options perm group address)
    (if (string-prefix? "/" address)
      ; set socket permissions and group if configured in config file
      (if (integer? perm) (chmod address perm))
      ; socket group setting can be tricky: in some circumstances or on some platforms it is not possible
      (if group (chown address -1 (if (string? group) (group:gid (getgrnam group)) group)))))

  (define (call-with-socket listen-address listen-port swa-env proc)
    (let (socket (server-create-bound-socket listen-address listen-port))
      (local-socket-set-options listen-address (swa-config-ref swa-env socket-permissions)
        (swa-config-ref swa-env socket-group))
      (proc socket)))

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
          (or (swa-config-ref swa-env listen-address)
            (if-pass (swa-config-ref swa-env socket-name)
              (l (a) (string-append (dirname scgi-default-address) "/" a)) scgi-default-address)))
        (listen-port (swa-config-ref swa-env listen-port)))
      (call-with-socket listen-address listen-port swa-env
        (l (socket) (start-message listen-address listen-port)
          (if (or (swa-config-ref swa-env single-threaded) (swa-config-ref swa-env development))
            ;single-threaded without extra exception handler
            (scgi-handle-requests (l (headers client) (http-respond headers client app-respond))
              socket 1)
            ;possibly multi-threaded and all exceptions catched for continuous processing
            (scgi-handle-requests (l (headers client) (http-respond headers client app-respond))
              socket (swa-config-ref swa-env workers) #f #f exception-handler exception-keys))
          (display "stopped listening. ")))))

  (define (swa-server-http swa-env swa-app)
    "list string/rnrs-hashtable false/procedure:{key resume exception-arguments ...} boolean/ (symbol ...) ->
     starts a server to process http requests. no scgi proxy needed.
     this uses guiles (web server) and has not been tested much yet"
    (apply
      (l (listen-address listen-port app-init app-respond app-deinit) (app-init)
        (call-with-socket listen-address listen-port
          swa-env
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
                        (string-append (apply string-append (swa-http-response-headers response))
                          "\r\n")))
                    (swa-http-response-body response))))
              (q http) (list #:socket socket))))
        (app-deinit))
      (or (swa-config-ref swa-env listen-address) "::1")
      (or (swa-config-ref swa-env listen-port) 8080) swa-app))

  ;-- other

  (define-syntax-rule (match-path path specs ...)
    ; split a path into parts and use ice-9 match to match parts
    (match (tail (path->list path)) specs ...)))
