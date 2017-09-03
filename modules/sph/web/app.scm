(library (sph web app)
  (export
    default-server-error-handler
    match-path
    scgi-headers-get-content-length
    sph-web-app-description
    swa-app-deinit
    swa-app-depends
    swa-app-init
    swa-app-name
    swa-app-record
    swa-app-respond
    swa-config-bind
    swa-config-ref
    swa-create
    swa-env-config
    swa-env-config-set!
    swa-env-data
    swa-env-data-set!
    swa-env-paths
    swa-env-record
    swa-env-root
    swa-env?
    swa-server-guile
    swa-server-internal
    swa-server-scgi
    swa-start
    swa-test-http-procedure-wrap
    swa-test-http-start)
  (import
    (ice-9 match)
    (sph base)
    (sph log)
    (sph record)
    (sph scgi)
    (sph server)
    (sph web app http)
    (sph web app start)
    (web http)
    (web request)
    (web response)
    (web server)
    (web uri)
    (only (guile) port-closed?))

  (define sph-web-app-description
    "main module that exports swa-start, swa-server-scgi and various helpers.
     # syntax
     swa-test-http-start :: projects config-name swa-app test-settings proc -> test-result
       start a sph web app application for use in a (sph test) module.
       sets the procedure-wrap test setting.
       example usage:
         (swa-test-http-start (sph-cms) \"default\"
         swa-app test-settings (l (test-settings swa-env) (test-execute-procedures test-settings tests)))")

  (define-syntax-rule (match-path path specs ...)
    ; split a path into parts and use ice-9 match to match parts
    (match (tail (path->list path)) specs ...))

  (define-syntax-rule (swa-config-ref swa-env key)
    ; get the value for an unquoted config key from a swa-env object.
    ; (swa-config-ref swa-env testkey)
    (ht-ref (swa-env-config swa-env) (quote key)))

  (define-syntax-rule (swa-config-bind swa-env (key ...) body ...)
    (ht-bind (swa-env-config swa-env) (key ...) body ...))

  ;-- server

  (define-syntax-rule (start-message address port)
    (display
      (string-append "listening on " address
        (if (and (not (string-prefix? "/" address)) port) (string-append ":" (number->string port))
          "")
        "\n")))

  (define (local-socket-set-options address perm group)
    (if (string-prefix? "/" address)
      (begin
        ; set socket permissions and group if configured in config file
        (if (integer? perm) (chmod address perm))
        ; socket group setting can be tricky: in some circumstances or on some platforms it is not possible
        (if group (chown address -1 (if (string? group) (group:gid (getgrnam group)) group))))))

  (define (call-with-socket listen-address listen-port perm group proc)
    (let (socket (server-create-bound-socket listen-address listen-port))
      (local-socket-set-options listen-address perm group) (proc socket)))

  (define (scgi-headers-get-content-length h)
    ; get the request-body-size from headers
    (alist-ref h "CONTENT_LENGTH"))

  (define (default-server-error-handler obj resume socket) (log-message (q error) obj)
    (if (port-closed? socket) #f (resume)))

  (define (swa-env? a) (and (vector? a) (= (vector-length a) (ht-size swa-env-record))))

  (define (call-app-init app-init swa-env)
    ; update only if result is a vector of specific length.
    ; this is to make it less likely that swa-env is updated unintentionally
    (let (a (app-init swa-env)) (if (swa-env? a) a swa-env)))

  (define swa-scgi-default-config
    (ht-create-symbol exception-resume #f
      listen-address #f
      listen-port #f socket-group #f socket-name #f socket-permissions 504 thread-count #f))

  (define (swa-scgi-default-address socket-name)
    (if socket-name (string-append (dirname scgi-default-address) "/" socket-name)
      scgi-default-address))

  (define (swa-server-socket swa-env swa-app prepare-server-config c)
    "vector vector procedure:{socket -> any} -> any
     initialise a sph web application and a socket to use and call c with the socket.
     when c returns, the application is deinitialised and the result is the result of c"
    (let*
      ( (swa-env (call-app-init (swa-app-init swa-app) swa-env))
        (app-deinit (swa-app-deinit swa-app)) (config (swa-env-config swa-env))
        (config (prepare-server-config (and config (ht-ref-q config server)))))
      (ht-bind config (listen-address listen-port socket-permissions socket-group)
        (call-with-socket listen-address listen-port
          socket-permissions socket-group
          (l (socket) (start-message listen-address listen-port)
            (begin-first (c socket config) (app-deinit swa-env) (display "stopped listening.")))))))

  (define*
    (swa-server-scgi swa-env swa-app #:key (parse-query? #t)
      (exception-handler default-server-error-handler))
    "vector vector #:parse-query? boolean #:exception-handler procedure:{obj resume ->} ->
     starts a server listens for scgi requests and calls app-respond for each new request.
     calls app-init once on startup and app-deinit when the server stops listening.
     app-init can return a new or updated swa-env.
     order:
       app-init
       start-server
     optional swa configuration options:
       server-thread-count integer
       server-exception-resume boolean
     this uses (sph scgi) which uses (sph server)"
    (swa-server-socket swa-env swa-app
      (l (config)
        (let (config (ht-copy* swa-scgi-default-config (l (a) (ht-merge! a config))))
          (ht-set-q! config listen-address
            (or (ht-ref-q config listen-address)
              (swa-scgi-default-address (ht-ref-q config socket-name))))
          config))
      (l (socket config)
        (let
          ( (app-respond (swa-app-respond swa-app))
            (http-respond (if parse-query? swa-http-respond-query swa-http-respond)))
          (ht-bind config (exception-resume thread-count)
            (scgi-handle-requests
              (l (headers client) (http-respond swa-env app-respond headers client)) socket
              thread-count #f #f (and exception-resume exception-handler)))))))

  (define (swa-server-guile swa-env swa-app)
    "vector vector ->
     starts a server to process http requests directly, without an scgi proxy.
     the swa-app will be responsible for the sending of any static files that might be requested"
    (swa-server-socket swa-env swa-app
      (l (config)
        (let (config (ht-copy* swa-scgi-default-config (l (a) (ht-merge! a config))))
          (ht-set-q! config listen-address (or (ht-ref-q config listen-address) "127.0.0.1")) config))
      (l (socket config)
        (let (app-respond (swa-app-respond swa-app))
          (run-server
            (l (request request-body)
              (let*
                ( (headers
                    ; normalise headers to have string keys. uri->string just returned http:/
                    (pairs (pair "request_uri" (uri-path (request-uri request)))
                      (pair "request_method"
                        (string-downcase (symbol->string (request-method request))))
                      (map (l (a) (pair (symbol->string (first a)) (tail a)))
                        (request-headers request))))
                  (response
                    (app-respond
                      ; create a swa-http-request object
                      (record swa-http-request (alist-ref headers "request_uri")
                        #f headers #f swa-env)))
                  (res-headers
                    (read-headers
                      (open-input-string
                        (string-append (apply string-append (swa-http-response-headers response))
                          "\r\n"))))
                  (res-body (swa-http-response-body response)))
                (values
                  (build-response #:headers res-headers #:code (swa-http-response-status response))
                  res-body)))
            (q http) (list #:socket socket))))))

  (define (swa-server-internal swa-env swa-app proc)
    "vector list procedure:{procedure:{any ... -> any}} -> any:procedure-result
     initialise the application, call proc like (proc respond), where respond
     is a procedure that takes an arbitrary number of arguments and will always
     implicitly pass swa-env as the first argument to app-respond.
     afterwards deinitialise and return the result of the call to proc"
    (list-bind swa-app (respond init deinit)
      (init swa-env) (begin-first (proc swa-env respond) (deinit swa-env))))

  (define (with-response-and-string get-response client path/headers c)
    (let*
      ( (headers
          (if (string? path/headers)
            (list (pair "request_uri" path/headers) (pair "request_method" "get")) path/headers))
        (response (get-response headers))
        (response-string
          (begin (swa-http-response-send response client) (get-output-string client))))
      (c response response-string)))

  (define (swa-test-http-procedure-wrap swa-env app-respond)
    "to be used for the procedure-wrap option in (sph test) settings.
     extends a (sph test) test procedure to call app-respond with a swa-http-request object like swa-server-scgi would.
     test procedures are expected to not be defined with define-test, but instead with the signature:
       test-mytest :: get-response a ... -> test-result
       test-mytest :: get-response arguments expected test-settings -> test-result
       get-response :: string:path/((string . string) ...):headers procedure:{swa-http-response response-string -> any} -> any
     it will otherwise behave as if defined like (define-test (mytest _ ...) _ ...).
     the first argument to the test procedure is a procedure that either takes a path or an association list of request headers,
     and a procedure that will be called with the response object and the full http response string that app-respond created"
    (l (test-proc)
      (l (arguments . a)
        (apply test-proc
          (l (path/headers c)
            (let (client (open-output-string))
              (with-response-and-string
                (l (headers) "list -> swa-http-response"
                  (swa-http-parse-query headers
                    (l (path arguments)
                      (app-respond (record swa-http-request path arguments headers client swa-env)))))
                client path/headers c)))
          swa-env a))))

  (define (swa-test-http-start web-app-load-paths projects config-name swa-app test-settings c)
    (swa-start web-app-load-paths projects
      config-name swa-server-internal
      swa-app
      (l (swa-env app-respond)
        (let
          ( (procedure-wrap (alist-ref test-settings (q procedure-wrap)))
            (swa-procedure-wrap (swa-test-http-procedure-wrap swa-env app-respond)))
          (c
            (alist-set test-settings (q procedure-wrap)
              (if procedure-wrap (swa-procedure-wrap procedure-wrap) swa-procedure-wrap))
            swa-env))))))
