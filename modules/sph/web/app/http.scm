(define-module (sph web app http))

(use-modules (srfi srfi-1) (srfi srfi-2)
  (rnrs io ports) (sph)
  (sph alist) (sph hashtable) (sph string) (sph vector) (sph web app client) (sph web http))

(export nginx-respond-file nginx-respond-file-download
  respond respond-type
  sph-web-app-http-description swa-http-create-response
  swa-http-header-content-type swa-http-parse-query
  swa-http-request-client swa-http-request-cookie
  swa-http-request-data swa-http-request-data-set!
  swa-http-request-headers swa-http-request-https?
  swa-http-request-if-modified-since swa-http-request-method
  swa-http-request-new swa-http-request-path
  swa-http-request-query swa-http-request-swa-env
  swa-http-respond swa-http-respond-query
  swa-http-response swa-http-response*
  swa-http-response-body swa-http-response-content-type-add
  swa-http-response-headers swa-http-response-headers-add!
  swa-http-response-send swa-http-response-send-body swa-http-response-status)

(define sph-web-app-http-description
  "http request/response helpers.
   exports a binding named \"respond\", should that conflict with other names use the renaming
   feature when importing rnrs libraries to add a prefix.
   # syntax
   respond
     short for swa-http-create-response
   respond-type
     signatures
       :: symbol:type-key procedure -> swa-http-response
       :: symbol:type-key integer:http-status-code (string ...):headers procedure -> swa-http-response
     description
       respond with a type available in swa-http-key->mime-type. json, html, text, css and js are available by default.
     examples
       (respond-type (quote text) responder)
       (respond-type (quote text) 200 headers responder)")

(define swa-http-request-path (vector-accessor 1))
(define swa-http-request-query (vector-accessor 2))
(define swa-http-request-headers (vector-accessor 3))
(define swa-http-request-client (vector-accessor 4))
(define swa-http-request-swa-env (vector-accessor 5))
(define swa-http-request-data (vector-accessor 6))
(define swa-http-request-data-set! (l (a value) (vector-set! a 6 value)))

(define (swa-http-request-new path query headers client swa-env data)
  (vector (q swa-http-request) path query headers client swa-env data))

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

(define (swa-http-parse-query headers c)
  "list procedure:{string:path ((key . value) ...):parsed-query-string -> any} -> any
   key/value pairs in the query string must be separated by ; (as recommended by the w3c instead of &).
   the header that contains the full url path must be available and have the lowercase name \"request_uri\""
  (let (url-path (alist-ref headers "request_uri"))
    (apply
      (l (path . arguments)
        (c path
          (if (null? arguments) arguments (http-uri-query-string->alist (first arguments) #\;))))
      (string-split url-path #\?))))

(define-syntax-rules swa-http-create-response ((a) (swa-http-create-response* a))
  ((status body) (swa-http-response-new status (list) body))
  ((status headers body) (swa-http-response-new status headers body)))

(define swa-http-response-status (vector-accessor 1))
(define swa-http-response-headers (vector-accessor 2))
(define swa-http-response-body (vector-accessor 3))
(define swa-http-response-headers-set! (vector-setter 2))

(define (swa-http-response-new status headers body)
  (vector (q swa-http-response) status headers body))

(define swa-http-key->mime-type
  (ht-create-symbol-q json "application/json"
    html "text/html" text "text/plain" css "test/css" js "text/javascript"))

(define* (swa-http-header-content-type key #:optional (encoding "utf-8"))
  "symbol:content-type-identifier -> string:header-line
   results in a header-line for setting the response content-type.
   key has to exist in swa-http-key->mime-type. by default json, html, text, css and js are available."
  (http-header-line "content-type"
    (string-append (if (string? key) key (ht-ref swa-http-key->mime-type key)) ";charset=" encoding)))

(define (swa-http-response-headers-add! a . header-lines)
  "vector:swa-http-response string:header-line ... -> vector:swa-http-response
   add header-lines to the response and result in the extended response"
  (swa-http-response-headers-set! a (append header-lines (swa-http-response-headers a))) a)

(define (swa-http-response-content-type-add a key)
  "vector:swa-http-response content-type-identifier -> vector:swa-http-response
   adds a header-line for setting the content-type to the response and results in the extended response.
   key has to exist in swa-http-key->mime-type. by default json, html, text, css and js are available."
  (swa-http-response-headers-add! a (swa-http-header-content-type key)))

(define (swa-http-create-response* a)
  "integer/vector/procedure:{port ->}/string/any -> vector:swa-http-response
   integer: http status code only, empty response-body
   vector: as is. likely a swa-http-response-record
   procedure: applied with a port where response data can be written to
   string: http 200, string as response-body
   boolean-false: 404
   else: http 200, empty response-body"
  (if a
    (cond
      ((procedure? a) (swa-http-create-response 200 (list) a))
      ((integer? a) (swa-http-create-response a (list) ""))
      ((vector? a) a)
      (else (swa-http-create-response 200 (list) (if (string? a) a ""))))
    (swa-http-create-response 404 (list) "")))

(define (swa-http-response-send-body a client)
  (if (procedure? a) (begin (put-char client #\newline) (a client))
    (if (string? a)
      (begin (put-string client (http-header-line "content-length" (string-octet-length a)))
        (put-char client #\newline) (put-string client a)))))

(define (swa-http-response-send response client)
  "vector:swa-http-response port ->
   sends a response with http syntax.
   swa-http-response-body can be a procedure, which will be applied with the client-port.
   this enables stream-like response-body sending"
  (http-write-status-line (swa-http-response-status response) client)
  (each (l (line) (put-string client line)) (swa-http-response-headers response))
  (swa-http-response-send-body (swa-http-response-body response) client))

(define (swa-http-respond swa-env app-respond headers client)
  "list:response-header:(string:header-line ...) port procedure:{string:uri list:headers port:client} ->
   receives a request and calls app-respond with a request object and sends the response"
  (swa-http-response-send
    (app-respond
      (swa-http-request-new (alist-ref headers "request_uri") #f headers client swa-env #f))
    client))

(define (swa-http-respond-query swa-env app-respond headers client)
  "vector procedure list port -> vector
   like swa-http-respond but parses an available url query string into an association list that is passed in the request object"
  (swa-http-response-send
    (swa-http-parse-query headers
      (l (path arguments)
        (app-respond (swa-http-request-new path arguments headers client swa-env #f))))
    client))

(define-syntax-rule (respond a ...) (swa-http-create-response a ...))

(define-syntax-rules respond-type
  ((type-key body) (respond 200 (list (swa-http-header-content-type type-key)) body))
  ( (type-key status header-lines body)
    (respond status (pair (swa-http-header-content-type type-key) header-lines) body)))

(define* (nginx-respond-file path #:optional mime-type)
  "string [string] -> swa-http-response
   adds an x-accel-redirect header, which is nginx' feature to advise it to send
   a file which should be faster and easier to do that managing the file sending in user code.
   respond with status code 200 and no additional (nginx will normally choose one) or the given mime-type.
   the path is always relative to a configured nginx location or root"
  (respond 200
    (append (list (http-header-line "x-accel-redirect" path))
      (if mime-type (list (http-header-line "content-type" mime-type)) (list)))
    ""))

(define* (nginx-respond-file-download path file-name #:optional mime-type)
  "string string [string] -> swa-http-response
   like nginx-respond-file but also adds a content-disposition header to suggest to the client
   that the file should be downloaded instead of displayed where possible"
  (respond 200
    (append
      (list (http-header-line "x-accel-redirect" path)
        (http-header-line "content-disposition"
          (string-append "attachment;filename=" (string-quote file-name))))
      (if mime-type (list (http-header-line "content-type" mime-type)) (list)))
    ""))
