(library (project-example main)
  (export
    swa-app)
  (import
    (project-example branch main)
    (project-example lib main)
    (sph base)
    (sph web app)
    (sph web app http))

  ; sph-web-app main modules are normal scheme libraries and should return
  ; a swa-app object that will be used in the server start script (exe/start)

  (define (app-init swa-env) swa-env)
  (define (app-exit swa-env) "unspecified")

  (define (app-respond request)
    "vector:swa-http-request -> vector:swa-http-response
     the main request handler for the application"
    ; match-path takes a string and matches it as a splitted path like ("usr" "lib") with (ice-9 match).
    ; for more options see the documentation for (ice-9 match).
    (match-path (swa-http-request-path request)
      ( ()
        (respond-html request
          ; template variables
          (alist (q title) "testtitle")
          ; templates or plain sxml composed with (sph template).
          ; paths can be relative to {project-root}/client/ and do not need to have a filename extension.
          ; note that the (div) is wrapped in another list because otherwise it would be interpreted for composition
          (list "lib/layout" (list (q (div "root path response"))))))
      (("main" id) (main-example request id)) (_ (respond "default route"))))

  ; app-init and app-exit are optional. (swa-create app-respond app-init) would also work
  (define swa-app (swa-create app-respond app-init app-exit)))
