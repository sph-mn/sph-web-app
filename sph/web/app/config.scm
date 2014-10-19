(library (sph web app config)
  (export
    swa-default-config
    swa-library-prefix
    swa-paths
    swa-project-name
    swa-root)
  (import
    (rnrs base)
    (sph hashtable))

  (define swa-default-config (symbol-hashtable config-name "default"))
  (define swa-paths #f)
  (define swa-root #f)
  (define swa-library-prefix)
  (define swa-project-name))