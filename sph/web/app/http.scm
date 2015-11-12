(library (sph web app http)
  (export
    swa-http-create-response
    swa-http-header-content-type
    swa-http-if-modified-since
    swa-http-key->mime-type
    swa-http-request-cookie
    swa-http-request-https?
    swa-http-request-method
    swa-http-respond
    swa-http-respond-parse-query-string
    swa-http-response
    swa-http-response*
    swa-http-response-body
    swa-http-response-body-set!
    swa-http-response-content-type-set
    swa-http-response-header
    swa-http-response-header-add!
    swa-http-response-header-set!
    swa-http-response-status
    swa-http-response-status-set!
    swa-http-send-response
    swa-http-send-response-body
    swa-http-split-query-string)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph conditional)
    (sph hashtable)
    (sph record)
    (sph web http)
    (only (sph alist) alist-ref)
    (only (sph string) string-equal? string-octet-length))

  (define swa-http-response-record (make-record-layout (q (status headers body))))

  (define-record-accessors swa-http-response-record (swa-http-response-status (q status))
    (swa-http-response-header (q headers)) (swa-http-response-body (q body)))

  (define-record-setters swa-http-response-record (swa-http-response-status-set! (q status))
    (swa-http-response-header-set! (q headers)) (swa-http-response-body-set! (q body)))

  (define-syntax-rule (swa-http-create-response status headers body) (vector status headers body))

  (define-syntax-rules swa-http-response ((a) (swa-http-response* a))
    ((status body) (swa-http-create-response status (list) body))
    ((status header body) (swa-http-create-response status header body)))

  (define-as swa-http-key->mime-type hashtable-quoted
    json "application/json"
    html "text/html" text "text/plain" style "test/css" script "text/javascript")

  (define* (swa-http-header-content-type key #:optional (encoding "utf-8"))
    "symbol:content-type-identifier -> string:header-line
    results in a header-line for setting the response content-type.
    key has to exist in swa-http-key->mime-type. by default json, html, text, style and script are available."
    (http-header-line "content-type"
      (string-append (hashtable-ref swa-http-key->mime-type key) ";charset=" encoding)))

  (define (swa-http-request-https? h)
    "alist:headers -> boolean
    results in a boolean indicating if the headers correspond to an https request.
    depends on the scgi header variable \"https\", which should have the value \"on\" when the request is an https request"
    (string-equal? "on" (alist-ref h "https")))

  (define (swa-http-response-content-type-add a key)
    "vector:swa-http-response content-type-identifier -> vector:swa-http-response
    adds a header-line for setting the content-type to the response and results in the extended response.
    key has to exist in swa-http-key->mime-type. by default json, html, text, style and script are available."
    (swa-http-response-header-add! a (swa-http-header-content-type key)))

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

  (define (swa-http-request-cookie headers) "alist -> alist:parsed-cookie"
    (and-let*
      ((header (alist-ref headers "http_cookie")) (parsed-cookie (http-parse-cookie-header header)))
      parsed-cookie))

  (define (swa-http-if-modified-since h)
    "headers -> integer:unix-time-seconds/false
    expects the header to be called \"if_modified_since\""
    (false-if-exception (pass-if (alist-ref h "if_modified_since") http-parse-date->seconds)))

  (define (swa-http-request-method headers)
    "alist -> symbol:http-method-name
    get the http method as a symbol from parsed headers.
    example http method names are get/post/put"
    (false-if-exception (string->symbol (string-downcase (alist-ref headers "request_method")))))

  (define (swa-http-response-header-add! a . header-lines)
    "vector:swa-http-response string:header-line ... -> vector:swa-http-response
    add header-lines to the response and result in the extended response"
    (swa-http-response-header-set! a (append header-lines (swa-http-response-header a))) a)

  (define (swa-http-respond-parse-query-string headers client app-respond)
    "list:response-header:(string:header-line ...) port procedure:{string:path list:parsed-url-query-string list:headers port:client} ->
    like swa-http-respond but calls app-respond with an additional argument for the parsed url query-string.
    key/value pairs in the query string should be separated by ; (as recommended in favor of & by the w3c).
    note that this procedure is not required for using query-strings and that it is possible to manually parse the query string from the request-url when needed
    (for example with swa-http-split-query-string)"
    (apply
      (l (path . arguments)
        (swa-http-send-response (app-respond path arguments headers client) client))
      (swa-http-split-query-string (alist-ref headers "request_uri"))))

  (define (swa-http-split-query-string url-path)
    "list:headers string -> (string . alist:((string . string) ...))"
    (apply
      (l (path . arguments)
        (pair path
          (if (null? arguments) arguments (http-uri-query-string->alist (first arguments) #\;))))
      (string-split url-path #\?)))

  (define (swa-http-respond headers client app-respond)
    "list:response-header:(string:header-line ...) port procedure:{string:uri list:headers port:client} ->
    receives a request and applies app-respond with request data to create a response"
    (swa-http-send-response (app-respond (alist-ref headers "request_uri") headers client) client))

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
    (each (l (line) (display line client)) (swa-http-response-header response))
    (swa-http-send-response-body (swa-http-response-body response) client)))