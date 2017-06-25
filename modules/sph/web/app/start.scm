(library (sph web app start)
  (export
    sph-web-app-start-description
    swa-config-get
    swa-create
    swa-env-config
    swa-env-data
    swa-env-data-set!
    swa-env-paths
    swa-env-record
    swa-env-root
    swa-start)
  (import
    (sph base)
    (sph record)
    (only (sph tree) tree-map-lists-and-self))

  (define* (swa-create respond #:optional init deinit)
    "procedure:{vector:request -> vector:response} false/procedure:{vector:swa-env ->} ... -> (false/procedure ...)
     create a swa-app object that encapsulates the applications respond creation, initialise and deinitialise procedure"
    (list respond (or init identity) (or deinit identity)))

  (define sph-web-app-start-description
    "core web app initialisation
     before using swa-start, import (sph web app) and the main module of the current project, for example with (import (sph web app) (myproject main))
     syntax of swa-start:
     (swa-start project-name/project-names configuration-name handler handler-arguments ...)
     (swa-start (symbol ...)/((symbol ...) ...) string procedure:{vector:swa-env any ... -> any} any ...)
     usage of swa-start:
     (swa-start (myproject) \"default\" swa-scgi)
     or
     (swa-start ((myproject) (mymodules animportedproject)) \"development\" swa-scgi)
     or
     (swa-start ((a) (b) (c d)) \"development\" swa-scgi)")

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
    "string string -> list
     get a hashtable for the configuration file identified by \"name\""
    (let (path (string-append swa-root "config/" name ".scm"))
      (tree-map-lists-and-self (compose ht-from-alist list->alist)
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

  (define-record swa-env root paths config data)
  (define swa-env-record swa-env)

  (define-syntax-cases swa-start s
    ; list string/hashtable procedure any ... -> handler-result
    ; get full paths for project names using the load path, create the swa-env and call handler.
    ( ( ( (projects ...) ...) config handler handler-arguments ...)
      (let*
        ( (projects-datum (syntax->datum (syntax ((projects ...) ...))))
          (paths-datum (swa-paths-get projects-datum))
          (paths (datum->syntax s (pair (q list) paths-datum)))
          (root (datum->syntax s (first paths-datum))))
        (swa-require-load-path paths-datum projects-datum)
        ; unfortunately, importing the main module and calling app-init/app-deinit did not
        ; work as it does not seem reasonably possible to make the bindings available here
        ; without run-time module loading.
        (quasisyntax
          (let ((swa-paths (unsyntax paths))) (apply swa-link-root-files swa-paths)
            (handler
              (record swa-env (unsyntax root)
                swa-paths
                (if (string? config) (swa-config-get (unsyntax root) config)
                  (or config (ht-create-symbol))))
              handler-arguments ...)))))
    ( (projects config handler handler-arguments ...)
      (syntax (swa-start (projects) config handler handler-arguments ...)))))
