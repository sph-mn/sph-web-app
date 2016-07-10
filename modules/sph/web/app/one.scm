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
    (sph web shtml))

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
          (http-header-line "content-disposition" (string-append "attachment;filename=" file-name)))
        (if mime-type (list (http-header-line "content-type" mime-type)) (list)))
      ""))

  (define (shtml-includes-proc sources-css sources-javascript) "usage: (shtml-include (q css) ref)"
    (let-syntax
      ( (get-sxml
          (syntax-rule (format ref sources client-file create-include-sxml)
            (map create-include-sxml
              (append (if-pass (apply client-file #f sources) list (list))
                (if-pass (ref format #f) list (list)))))))
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
