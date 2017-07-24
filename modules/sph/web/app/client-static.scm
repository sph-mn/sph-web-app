

(define (client-file-static-compile swa-env)
    "vector -> unspecified
     pre-compile the static client files to be served"
    (let (data (swa-env-data swa-env))
      (ht-each
        (l (output-format config)
          (ht-each
            (l (key bindings-and-sources)
              (ht-tree-set! data output-format
                key (apply client-file output-format swa-env bindings-and-sources)))))
        client-file-static)))
