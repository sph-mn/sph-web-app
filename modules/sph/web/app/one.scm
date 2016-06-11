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

  (define (sxhtml-includes-proc sources-css sources-javascript)
    "usage: (sxhtml-include (q css) ref)"
    (let-syntax
      ( (get-sxml
          (syntax-rule (format ref sources client-file create-include-sxml)
            (map create-include-sxml
              (append (if-pass (apply client-file #f sources) list (list))
                (if-pass (ref format #f) list (list)))))))
      (l (format ref)
        (if (equal? (q css) format)
          (get-sxml format ref sources-css client-css-file sxhtml-include-css)
          (get-sxml format ref sources-javascript client-javascript-file sxhtml-include-javascript)))))

  (define (call-hook hook-procedures hook-name . a)
    "rnrs-hashtable symbol procedure-arguments ... -> any
    if a procedure with hook-name exists in hook-procedures, then it is applied with \"a\"
    if not, \"a\" is the result"
    (if hook-procedures
      (let (proc (hashtable-ref hook-procedures hook-name)) (if proc (apply proc a) a)) a)))
