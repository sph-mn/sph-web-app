(define-test-module (test module sph web app client)
  (import
    (sph io)
    (sph list)
    (sph record)
    (sph hashtable)
    (sph web app client)
    (sph web app start))

  ; tests currently depend on csstidy and uglifyjs being available in PATH

  (define swa-env
    (record swa-env-record "/tmp/"
      (ht-create-symbol-q test "/tmp/") (ht-create-symbol-q mode (q production)) (ht-create-symbol)))

  (define-test (client-html)
    (assert-equal "<!doctype html><div>test</div>\n"
      (call-with-output-string
        (l (out) (client-html swa-env out #f (q test) (list-q ((div "test"))))))))

  (define-test (client-css)
    (assert-equal "div{display:none;}\n"
      (call-with-output-string
        (l (out) (client-css swa-env out #f (q test) (list-q ((("div" display none)))))))))

  (define-test (client-js)
    (assert-equal "\"use strict\";var a=3;\n"
      (call-with-output-string
        (l (out) (client-js swa-env out #f (q test) (list (list (list-q (define a 3)))))))))

  (define-test (client-file)
    (and-let* ((path (client-file-html swa-env #f (q test) (list-q ((div "test"))))))
      (assert-equal "<!doctype html><div>test</div>\n"
        (file->string (string-append (swa-env-root swa-env) "root" path)))))

  (test-execute-procedures-lambda client-file client-js client-css client-html))
