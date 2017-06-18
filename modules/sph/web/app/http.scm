(library (sph web app http)
  (export
    nginx-respond-file
    nginx-respond-file-download
    respond
    respond-html
    respond-type
    sph-web-app-http-description
    swa-http-create-response
    swa-http-header-content-type
    swa-http-request-cookie
    swa-http-request-https?
    swa-http-request-if-modified-since
    swa-http-request-method
    swa-http-request-query-parse
    swa-http-respond
    swa-http-response
    swa-http-response*
    swa-http-response-body
    swa-http-response-content-type-add
    swa-http-response-headers
    swa-http-response-headers-add!
    swa-http-response-status
    swa-http-send-response
    swa-http-send-response-body)
  (import
    (sph base)
    (sph record)
    (sph web app client)
    (sph web http))

  (define sph-web-app-http-description "http request/response helpers")
  ;
  ;-- request
  ;
  (define-record swa-http-request path query headers client swa-env)

  (define (swa-http-request-cookie request) "vector -> alist:parsed-cookie"
    (and-let*
      ( (headers (alist-ref (swa-http-request-headers request) "http_cookie"))
        (parsed-cookie (http-parse-cookie-header headers)))
      parsed-cookie))

  (define (swa-http-request-method request)
    "alist -> symbol:http-method-name
     get the http method as a symbol from parsed headers.
     example http method names are get/post/put"
    (false-if-exception
      (string->symbol
        (string-downcase (alist-ref (swa-http-request-headers request) "request_method")))))

  (define (swa-http-request-https? request)
    "alist:headers -> boolean
     results in a boolean indicating if the headers correspond to an https request.
     depends on the scgi header variable \"https\", which should have the value \"on\" when the request is an https request"
    (string-equal? "on" (alist-ref (swa-http-request-headers request) "https")))

  (define (swa-http-request-if-modified-since request)
    "headers -> integer:unix-time-seconds/false
     expects the header to be called \"if_modified_since\""
    (false-if-exception
      (and-let* ((a (alist-ref (swa-http-request-headers request) "if_modified_since")))
        (http-parse-date->time a))))

  (define (swa-http-request-query-parse request c)
    "vector:swa-http-request procedure:{string:path list:parsed-url-query-string -> any} -> any
     like swa-http-respond but calls app-respond with an additional argument for the parsed url query-string.
     key/value pairs in the query string should be separated by ; (as recommended in favor of & by the w3c).
     note that this procedure is not required for using query-strings and that it is possible to manually parse the query string from the request-url when needed
     (for example with swa-http-split-query-string)"
    (let (url-path (alist-ref (swa-http-request-headers request) "request_uri"))
      (apply
        (l (path . arguments)
          (c path
            (if (null? arguments) arguments (http-uri-query-string->alist (first arguments) #\;))))
        (string-split url-path #\?))))

  ;-- response
  ;

  (define-syntax-rules swa-http-create-response ((a) (swa-http-create-response* a))
    ((status body) (vector status (list) body)) ((status headers body) (vector status headers body)))

  (define-record swa-http-response status headers body)

  (define-as swa-http-key->mime-type symbol-hashtable
    json "application/json"
    html "text/html" text "text/plain" style "test/css" script "text/javascript")

  (define* (swa-http-header-content-type key #:optional (encoding "utf-8"))
    "symbol:content-type-identifier -> string:header-line
     results in a header-line for setting the response content-type.
     key has to exist in swa-http-key->mime-type. by default json, html, text, style and script are available."
    (http-header-line "content-type"
      (string-append (hashtable-ref swa-http-key->mime-type key) ";charset=" encoding)))

  (define (swa-http-response-headers-add! a . header-lines)
    "vector:swa-http-response string:header-line ... -> vector:swa-http-response
     add header-lines to the response and result in the extended response"
    (swa-http-response-headers-set! a (append header-lines (swa-http-response-headers a))) a)

  (define (swa-http-response-content-type-add a key)
    "vector:swa-http-response content-type-identifier -> vector:swa-http-response
     adds a header-line for setting the content-type to the response and results in the extended response.
     key has to exist in swa-http-key->mime-type. by default json, html, text, style and script are available."
    (swa-http-response-headers-add! a (swa-http-header-content-type key)))

  (define (swa-http-response* a)
    "integer/vector/procedure:{port ->}/string/any -> vector:swa-http-response
     integer: http status code only, empty response-body
     vector: as is. likely a swa-http-response-record
     procedure: applied with a port where response data can be written to
     string: http 200, string as response-body
     boolean-false: 404
     else: http 200, empty response-body"
    (if a
      (if (procedure? a) (swa-http-create-response 200 (list) a)
        (if (integer? a) (swa-http-create-response a (list) "")
          (if (vector? a) a (swa-http-create-response 200 (list) (if (string? a) a "")))))
      (swa-http-create-response 404 (list) "")))

  (define (swa-http-send-response-body a client)
    (if (procedure? a) (begin (display "\n" client) (a client))
      (if (string? a)
        (begin (display (http-header-line "content-length" (string-octet-length a)) client)
          (display "\n" client) (display a client)))))

  (define (swa-http-send-response response client)
    "vector:swa-http-response port ->
     sends a response with http syntax.
     swa-http-response-body can be a procedure, which will be applied with the client-port.
     this enables stream-like response-body sending"
    (http-write-status-line (swa-http-response-status response) client)
    (each (l (line) (display line client)) (swa-http-response-headers response))
    (swa-http-send-response-body (swa-http-response-body response) client))

  (define (swa-http-respond headers client app-respond)
    "list:response-header:(string:header-line ...) port procedure:{string:uri list:headers port:client} ->
     receives a request and applies app-respond with request data to create a response"
    (swa-http-send-response (app-respond (alist-ref headers "request_uri") headers client) client))

  (define-syntax-rule (respond a ...) (swa-http-response a ...))

  (define-syntax-rules respond-type
    ((type-key body) (respond 200 (list (swa-http-header-content-type type-key)) body))
    ( (type-key status header-lines body)
      (respond status (pair (swa-http-header-content-type type-key) header-lines) body)))

  (define* (respond-html bindings source #:optional (headers (list)))
    (respond 200 (pair "content-type:text/html\r\n" headers)
      (l (client) (client-html client bindings source))))

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
      "")))
