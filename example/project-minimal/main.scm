(library (project-minimal main)
  (export
    swa-app)
  (import
    (rnrs base)
    (sph web app)
    (sph web app http))

  (define (app-respond request) (respond "running"))
  (define swa-app (swa-create app-respond)))
