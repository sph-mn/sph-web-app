(library (project-name init)
  (export
    app-exit
    app-init
    app-respond)
  (import
    (rnrs base)
    (sph)
    (sph web app))

  (define (app-init) "unspecified")
  (define (app-exit) "unspecified")

  (define (app-respond path h client)
    (match-path path
      ( ()
        (respond-html (symbol-hashtable title "testtitle")
          (list (list "lib/client/default-wrap" (q (div "root path response"))))))
      (_ (respond "default route")))))