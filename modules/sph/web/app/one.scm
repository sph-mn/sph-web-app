(library (sph web app one)
  (export
    call-hook
    nginx-respond-file
    nginx-respond-file-download
    sxhtml-includes-proc)
  (import
    (rnrs base)
    (sph)
    (sph conditional)
    (sph documentation)
    (sph hashtable)
    (sph web app)
    (sph web http)
    (sph web sxhtml))

  (define (nginx-respond-file path mime-type)
    "the path is always relative to a configured nginx location"
    (respond 200
      (list (http-header-line "x-accel-redirect" path) (http-header-line "content-type" mime-type))
      ""))

  (define (nginx-respond-file-download path file-name mime-type)
    "like nginx-respond-file but adds a content-disposition header to suggest to the client
    that it should be treated as a download even if the client can display it"
    (respond 200
      (list (http-header-line "x-accel-redirect" path) (http-header-line "content-type" mime-type)
        (http-header-line "content-disposition" (string-append "attachment;filename=" file-name)))
      ""))

  (define (sxhtml-includes-proc static-css dynamic-css static-javascript dynamic-javascript)
    "creates a procedure that serves or processes given some assets by default, and additional ones given as arguments.
    dynamic means templates that are compiled, static are just path references
    initialisation: (define sxhtml-include (sxhtml-includes-proc (list) (list \"lib/client/one\") (list) (list)))
    usage: (sxhtml-include (q css) ref)"
    (let
      ( (static-css (map sxhtml-include-css static-css))
        (static-javascript (map sxhtml-include-javascript static-javascript)))
      (let-syntax
        ( (get-sxml
            (syntax-rule (format ref static dynamic client-file create-include-sxml)
              (append static
                (map create-include-sxml
                  (append
                    (if (null? dynamic) dynamic
                      (if-pass (apply client-file #f dynamic) list (list)))
                    (if-pass (ref format #f) list (list))))))))
        (l (format ref)
          (if (equal? (q css) format)
            (get-sxml format ref static-css dynamic-css client-css-file sxhtml-include-css)
            (get-sxml format ref
              static-javascript dynamic-javascript client-javascript-file sxhtml-include-javascript))))))

  (define (call-hook hook-procedures hook-name . a)
    "rnrs-hashtable symbol procedure-arguments ... -> any
    if a procedure with hook-name exists in hook-procedures, then it is applied with \"a\"
    if not, \"a\" is the result"
    (if hook-procedures
      (let (proc (hashtable-ref hook-procedures hook-name)) (if proc (apply proc a) a)) a)))
