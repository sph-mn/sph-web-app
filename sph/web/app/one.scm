(library (sph web app one)
  (export
    call-hook
    nginx-respond-file
    nginx-respond-file-download
    sxml-html-includes-proc)
  (import
    (rnrs base)
    (sph)
    (sph documentation)
    (sph hashtable)
    (sph web app)
    (sph web http)
    (sph web sxml-html))

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

  (define (sxml-html-includes-proc static-style dynamic-style static-script dynamic-script)
    "creates a procedure that serves or processes given assets by default, and additional ones if given as arguments. dynamic means templates that are compiled, static are just path references
    initialisation: (define sxml-html-include (sxml-html-includes-proc (list) (list \"lib/client/one\") (list) (list)))
    usage: (sxml-html-include (q style) ref)"
    (let
      ( (static-style (map sxml-html-include-style static-style))
        (static-script (map sxml-html-include-script static-script)))
      (let-syntax
        ( (get-sxml
            (syntax-rule (type ref static dynamic prepare-dynamic create-include-sxml)
              (append static
                (map create-include-sxml
                  (append (apply prepare-dynamic #f dynamic) (ref type (list))))))))
        (l (type ref)
          (if (eqv? (q style) type)
            (get-sxml type ref static-style dynamic-style client-style sxml-html-include-style)
            (get-sxml type ref static-script dynamic-script client-script sxml-html-include-script))))))

  (define (call-hook hook-procedures hook-name . a)
    "rnrs-hashtable symbol procedure-arguments ... -> any
    if a procedure with hook-name exists in hook-procedures, then it is applied with \"a\"
    if not, \"a\" is the result"
    (if hook-procedures
      (let (proc (hashtable-ref hook-procedures hook-name)) (if proc (apply proc a) a)) a)))
