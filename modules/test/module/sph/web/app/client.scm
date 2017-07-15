(define-test-module (test module sph web app client)
  (import
    (sph base)
    (sph record)
    (sph web app client)
    (sph web app start))

  ; tests currently depend on csstidy and uglifyjs being available in PATH

  (define swa-env
    (record swa-env-record "/tmp/"
      (list "/tmp/") (ht-create-symbol mode (q production)) (ht-create-symbol)))

  (define-test (client-html)
    (assert-equal "<!doctype html><div>test</div>"
      (call-with-output-string (l (out) (client-html swa-env out #f (list-q ((div "test"))))))))

  (define-test (client-css)
    (assert-equal "div{display:none;}\n"
      (call-with-output-string
        (l (out) (client-css swa-env out #f (list-q ((("div" display none)))))))))

  (define-test (client-javascript)
    (assert-equal "var a=3;\n"
      (call-with-output-string
        (l (out) (client-javascript swa-env out #f (list (list (list-q (define a 3)))))))))

  (define-test (client-file)
    (and-let* ((path (client-html-file swa-env #f (list-q ((div "test"))))))
      (assert-equal "<!doctype html><div>test</div>"
        (file->string (string-append (swa-env-root swa-env) "root" path)))))

  (test-execute-procedures-lambda client-file client-javascript client-css client-html))
