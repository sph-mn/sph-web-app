(library (sph web app start)
  (export
    swa-start)
  (import
    (sph base)
    (sph record)
    (only (sph tree) tree-map-lists-and-self))

  (define sph-web-app-start "web app initialisation")

  (define (swa-link-root-files swa-root . paths)
    "symlink non-generated files from the imports root-directories into the current root.
     currently requires that the filesystem supports symlinks. symlinks are also not deleted if the files are removed"
    (let (dest (string-append swa-root "root")) (ensure-directory-structure dest)
      (each
        (l (other-root)
          (shell-eval
            (string-append
              "cp --recursive --no-clobber --dereference --symbolic-link --target-directory=" dest
              (string-append other-root "root/*") " 2> /dev/null")))
        paths)))

  (define (swa-config-get swa-root name)
    ; string string -> list
    (let (path (string-append swa-root "config/" name ".scm"))
      (tree-map-lists-and-self (compose alist->hashtable list->alist)
        (primitive-eval (list (q quasiquote) (file->datums path))))))

  (define-syntax-rule (swa-paths-get projects)
    ; ((symbol ...) ...) -> (string ...)
    ; convert project names to full paths.
    ; the current project is included
    (filter-map
      (l (a)
        (let (relative-path (string-join (map symbol->string a) "/"))
          (any
            (l (load-path)
              (let (path (string-append load-path "/" relative-path))
                (and (file-exists? path) (ensure-trailing-slash path))))
            %load-path)))
      projects))

  (define-syntax-rule (swa-require-load-path paths-datum projects-datum)
    (if (null? paths-datum)
      (raise
        (list (q project-not-in-load-path) "this is required for loading application parts"
          (q search-paths) %load-path (q projects) projects-datum))))

  (define-syntax-rule (swa-import-main name) (qq (import ((unquote-splicing name) main))))
  ;(define-record swa-request path query headers client swa-root swa-paths swa-config)
  (define-record swa-env root paths config)

  (define-syntax-cases swa-start s
    ( ( ( (projects ...) ...) config-name handler handler-arguments ...)
      (let*
        ( (projects-datum (syntax->datum (syntax ((projects ...) ...))))
          (paths-datum (swa-paths-get projects-datum))
          (paths (datum->syntax s (pair (q list) paths-datum)))
          (root (datum->syntax s (first paths-datum))))
        (swa-require-load-path paths-datum projects-datum)
        (quasisyntax
          (let ((swa-paths (unsyntax paths))) (apply swa-link-root-files swa-paths)
            ;(app-init)
            (handler
              (record swa-env (unsyntax root)
                swa-paths (swa-config-get (unsyntax root) config-name))
              handler-arguments ...)
            ;(app-deinit)
            ))))
    ( (projects config-name handler handler-arguments ...)
      (syntax (swa-start (projects) config-name handler handler-arguments ...)))))
