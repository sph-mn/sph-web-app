(define-test-module (project-example test module main)
  (import
    (sph base)
    (project-example main)
    (sph web app)
    (sph web app http))

  (define (test-default-route get-res . a)
    "test procedures called in the context of swa-test-http-start
     are called with a \"get-response\" procedure.
     they do not have the typical (sph test) signature and therefore
     should not be defined with define-test"
    (get-res "/"
      (l (res res-string)
        (assert-and (assert-equal 200 (swa-http-response-status res))
          (assert-true (string? res-string))))))

  (define-as config ht-create-symbol
    start-content-ide #f
    preview-image-size 256
    browse-default-type "text" browse-page-max-entries 75 socket-permissions 504 mode (q development))

  (define-procedure-tests tests default-route)

  (l (settings)
    ; execute tests with swa-server-internal and special test procedures.
    (swa-test-http-start (project-example) config
      swa-app settings (l (settings swa-env) (test-execute-procedures settings tests)))))
