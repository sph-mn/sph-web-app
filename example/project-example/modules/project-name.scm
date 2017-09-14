(library (project-name)
  (export
    swa-app)
  (import
    (project-name http main)
    (sph)
    (sph web app)
    (sph web app http))

  ; sph-web-app main modules are normal scheme libraries and should return
  ; a swa-app object that will be used to start the server like in exe/start

  (define-as client-static-config client-static-config-create
    ; this configures client file bundles that are eventually compiled at start
    project-name
    (default js (#f "main") css ((example-variable "testvalue") "main"))
    (otherbundle js (#f "otherfile")))

  (define (app-init swa-env) swa-env)
  (define (app-exit swa-env) "unspecified")

  (define (app-respond request)
    "vector:swa-http-request -> vector:swa-http-response
     the main request handler of the application"
    ; match-path takes a string and matches it as a splitted path like ("usr" "lib") with (ice-9 match).
    ; for more options see the documentation for (ice-9 match).
    (match-path (swa-http-request-path request)
      ( ()
        (respond-html request
          ; template variables
          (alist (q title) "testtitle")
          ; templates or plain sxml composed with (sph template).
          ; the (div) is wrapped in another list because on this nesting level it would
          ; otherwise be appended to lib/layout
          (list "lib/layout" (list (q (div "root path response"))))))
      (("main" id) (main-example request id)) (_ (respond "default route"))))

  ; app-init and app-deinit are optional

  (define swa-app
    (swa-create (quote project-name) app-respond #:app-init app-init #:app-exit app-exit)))
