(library (project-example branch main)
  (export
    main-example)
  (import
    (project-example lib main)
    (sph)
    (sph web app)
    (sph web app http))

  ; branches provide features for url paths. for example all paths under /browse might be handled by one branch module.
  ; similar to controllers in mvc web frameworks.
  ; branch libraries are normal scheme libraries and merely a suggested way to structure a web application.

  (define (main-example request id)
    (respond (string-append "main branch response. received argument \"" id "\""))))
