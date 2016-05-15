(library (project-name main)
  (export
    app-respond)
  (import
    (rnrs base)
    (sph web app))

  (define (app-respond path h client)
    (respond "running")))
