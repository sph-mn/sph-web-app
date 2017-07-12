(define-test-module (test module sph web app client)
  (import
    (sph base)
    (sph record)
    (sph web app client)
    (sph web app start))

  (define swa-env
    (record swa-env-record "/tmp/" (list "/tmp/") (ht-create-symbol) (ht-create-symbol)))

  ;(client-html swa-env port bindings . sources)

 (define-test (client-html)
    (assert-equal "<!doctype html><div>test</div>"
      (call-with-output-string
        (l (out) (client-html swa-env out #f (list (list (q (div "test")))))))))


  (define-test (client-css)
    (assert-equal ""
      (call-with-output-string
        (l (out) (client-css swa-env out #f (list (list (q (div "test")))))))))

  (test-execute-procedures-lambda client-html client-css))
