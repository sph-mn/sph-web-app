(library (sph web app one)
  (export
    call-hook
    nginx-respond-file
    nginx-respond-file-download
    shtml-includes-proc)
  (import
    (rnrs base)
    (sph)
    (sph conditional)
    (sph documentation)
    (sph hashtable)
    (sph web app)
    (sph web http)
    (sph web shtml)
    (only (sph list) any->list)
    (only (sph string) string-quote))

  (define (default-server-error-handler obj resume socket) (log-message (q error) obj)
    (if (port-closed? socket) #f (resume)))

(define-syntax-rule (start-message address port)
    (display
      (string-append "listening on " address
        (if (and (not (string-prefix? "/" address)) port) (string-append ":" (number->string port))
          "")
        "\n")))

  (define*
    (swa-scgi swa-env #:key (http-respond swa-http-respond)
      (exception-handler default-server-error-handler)
      (exception-keys #t))
    "vector procedure:{headers client app-respond} false/procedure:{key resume exception-arguments ...} boolean/(symbol ...) ->
     starts a server to process scgi requests.
     this uses (sph scgi) which uses (sph server).
     currently the given exception handlers are only installed when the \"development\" config option is unset or false"
    (debug-log swa-env)
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

  (define-syntax-rules respond-type
    ((type-key body) (respond 200 (list (swa-http-header-content-type type-key)) body))
    ( (type-key status header-lines body)
      (respond status (pair (swa-http-header-content-type type-key) header-lines) body)))

  (define (headers-content-length h)
    ; get the request-body-size from the scgi-headers
    (alist-ref h "CONTENT_LENGTH"))

  (define-syntax-rule (respond a ...) (swa-http-response a ...))

  (define* (respond-html bindings source #:optional (headers (list)))
    (respond 200 (pair "content-type:text/html\r\n" headers)
      (l (client) (client-html client bindings source))))

  (define-syntax-rule (match-path path specs ...) (match (tail (path->list path)) specs ...))

  (define* (nginx-respond-file path #:optional mime-type)
    "the path is always relative to a configured nginx location or root"
    (respond 200
      (append (list (http-header-line "x-accel-redirect" path))
        (if mime-type (list (http-header-line "content-type" mime-type)) (list)))
      ""))

  (define* (nginx-respond-file-download path file-name #:optional mime-type)
    "like nginx-respond-file but adds a content-disposition header to suggest to the client
     that it should be treated as a download even if the client can display it"
    (respond 200
      (append
        (list (http-header-line "x-accel-redirect" path)
          (http-header-line "content-disposition"
            (string-append "attachment;filename=" (string-quote file-name))))
        (if mime-type (list (http-header-line "content-type" mime-type)) (list)))
      ""))

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
