(library (project-name http main)
  (export
    main-action)
  (import
    (sph common)
    (sph web app)
    (sph web app http))

  (define (main-action request id)
    (respond-html request
      (alist-q
        title "test title"
        ; include the pre-processed css file for the bundles default
        css (client-static swa-env (q project-name) (q css) (list-q default)))
      ; use the template file layout and insert example content
      (list "layout" (list "example content")))))
