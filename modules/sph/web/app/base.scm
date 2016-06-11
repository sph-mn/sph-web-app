(library (sph web app base)
  (export
    swa-default-config
    swa-library-prefix
    swa-paths
    swa-project-name
    swa-root
    swa-search-load-paths)
  (import
    (rnrs base)
    (sph)
    (sph hashtable)
    (only (guile) file-exists?))

  (define swa-default-config (symbol-hashtable config-name "default"))
  (define swa-paths #f)
  (define swa-root #f)
  (define swa-library-prefix)
  (define swa-project-name)

  (define (swa-search-load-paths a)
    (any (l (e) (let (path (string-append e a)) (if (file-exists? path) path #f))) swa-paths)))
