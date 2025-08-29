(define-test-module (test module sph web app client)
  (import (srfi srfi-2) (sph io) (sph list) (sph hashtable) (sph web app client) (sph web app))

  "tests currently depend on csstidy and uglifyjs being available in PATH"

  (define swa-env
    (swa-env-new "/tmp/" (ht-create-symbol-q mode (q production)) (ht-create-symbol-q test "/tmp/")))

  (define-test (client-html)
    (assert-equal "<!doctype html><div>test</div>\n"
      (call-with-output-string (l (out) (client-port swa-env (q html) out #f (q (((div "test")))))))))

  (define-test (client-css)
    (assert-equal "div{display:none;}\n"
      (call-with-output-string
        (l (out) (client-port swa-env (q css) out #f (q ((("div" display none)))))))))

  (define-test (client-js)
    (assert-equal "var a=3;\n"
      (call-with-output-string (l (out) (client-port swa-env (q js) out #f (q (((define a 3)))))))))

  (define-test (client-file)
    (and-let* ((path (client-file swa-env (q html) #f (q (((div "test")))))))
      (assert-equal "<!doctype html><div>test</div>\n"
        (file->string (string-append (swa-env-root swa-env) "webroot" path)))))

  (test-execute-procedures-lambda client-file client-html client-css client-js))
