(library (sph web app)
  (export
    default-server-error-handler
    match-path
    scgi-headers-get-content-length
    sph-web-app-description
    swa-config-bind
    swa-config-ref
    swa-create
    swa-env-config
    swa-env-data
    swa-env-data-set!
    swa-env-paths
    swa-env-record
    swa-env-root
    swa-env?
    swa-server-http
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
      ; set socket permissions and group if configured in config file
      (if (integer? perm) (chmod address perm))
      ; socket group setting can be tricky: in some circumstances or on some platforms it is not possible
      (if group (chown address -1 (if (string? group) (group:gid (getgrnam group)) group)))))

  (define (call-with-socket listen-address listen-port perm group proc)
    (let (socket (server-create-bound-socket listen-address listen-port))
      (local-socket-set-options listen-address perm group) (proc socket)))

  (define (scgi-headers-get-content-length h)
    ; get the request-body-size from headers
    (alist-ref h "CONTENT_LENGTH"))

  (define (default-server-error-handler obj resume socket) (log-message (q error) obj)
    (if (port-closed? socket) #f (resume)))

  (define (swa-env? a) (and (vector? a) (= (vector-length a) (ht-size swa-env-record))))

  (define-syntax-rule (call-app-init app-init swa-env)
    ; update only if result is a vector of specific length.
    ; this is to make it less likely that swa-env is updated unintentionally
    (let (a (app-init swa-env)) (if (swa-env? a) a swa-env)))

  (define*
    (swa-server-scgi swa-env swa-app #:key (parse-query? #t)
      (exception-handler default-server-error-handler))
    "vector procedure:{headers client app-respond} false/procedure:{key resume exception-arguments ...} boolean/(symbol ...) ->
     starts a server listens for scgi requests and calls app-respond for each new request.
     calls app-init once on startup and app-deinit when the server stops listening.
     app-init can return a new or updated swa-env.
     the no exception handler is installed when \"mode\" is development or \"single-threaded\" is true.
     this uses (sph scgi) which uses (sph server)"
    (swa-config-bind swa-env
      (listen-address socket-name listen-port
        socket-permissions socket-group mode single-threaded thread-count)
      (let
        ( (listen-address
            (or listen-address
              (if socket-name (string-append (dirname scgi-default-address) "/" socket-name)
                scgi-default-address)))
          (development (eq? mode (q development)))
          (http-respond (if parse-query? swa-http-respond-query swa-http-respond)))
        (call-with-socket listen-address listen-port
          socket-permissions socket-group
          (list-bind swa-app (app-respond app-init app-deinit)
            (l (socket)
              (let (swa-env (call-app-init app-init swa-env))
                (start-message listen-address listen-port)
                (if (or single-threaded development)
                  ;single-threaded without extra exception handler
                  (scgi-handle-requests
                    (l (headers client) (http-respond swa-env app-respond headers client)) socket
                    1 #f #f (if development #f exception-handler))
                  ;possibly multi-threaded and all exceptions catched for continuous processing
                  (scgi-handle-requests
                    (l (headers client) (http-respond swa-env app-respond headers client)) socket
                    thread-count #f #f exception-handler))
                (app-deinit swa-env) (display "stopped listening."))))))))

  (define (swa-server-http swa-env swa-app)
    "list string/rnrs-hashtable false/procedure:{key resume exception-arguments ...} boolean/ (symbol ...) ->
     starts a server to process http requests. no scgi proxy needed.
     this uses guiles (web server) and has not been tested much yet"
    (swa-config-bind swa-env (listen-address listen-port socket-permissions socket-group)
      (let (listen-address (or listen-address "::1"))
        (call-with-socket listen-address listen-port
          socket-permissions socket-group
          (list-bind swa-app (app-respond app-init app-deinit)
            (l (socket)
              (let (swa-env (call-app-init app-init swa-env))
                (start-message listen-address listen-port)
                (run-server
                  (l (request request-body)
                    (let*
                      ( (headers
                          (pair (pair "request_uri" (uri->string (request-uri request)))
                            (map (l (a) (pair (symbol->string (first a)) (tail a)))
                              (request-headers request))))
                        (response
                          (app-respond
                            (record swa-http-request (alist-ref headers "request_uri")
                              #f headers #f swa-env))))
                      (values
                        (read-headers
                          (open-input-string
                            (string-append
                              (apply string-append (swa-http-response-headers response)) "\r\n")))
                        (swa-http-response-body response))))
                  (q http) (list #:socket socket))
                (display "stopped listening.") (app-deinit swa-env))))))))

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

  (define-syntax-rule (swa-test-http-start projects config-name swa-app test-settings c)
    (swa-start projects config-name
      swa-server-internal swa-app
      (l (swa-env app-respond)
        (let
          ( (procedure-wrap (alist-ref test-settings (q procedure-wrap)))
            (swa-procedure-wrap (swa-test-http-procedure-wrap swa-env app-respond)))
          (c
            (alist-set test-settings (q procedure-wrap)
              (if procedure-wrap (swa-procedure-wrap procedure-wrap) swa-procedure-wrap))
            swa-env))))))
