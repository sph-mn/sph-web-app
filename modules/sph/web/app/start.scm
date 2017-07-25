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
    swa-project-id->symbol
    swa-start
    swa-start-p)
  (import
    (sph base)
    (sph record)
    (only (sph tree) tree-map-lists-and-self))

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

  (define (swa-project-id->symbol a)
    "(symbol ...) -> symbol
     if project-id is not a symbol but a list with symbols, join the list elements separated by \"-\" to create a new symbol.
     this is done to be able to use eq? with the resulting identifier, list literals did not work"
    (string->symbol (string-join (map symbol->string a) "-")))

  (define (swa-project-id->relative-path a) (string-join (map symbol->string a) "/"))

  (define* (swa-create respond #:optional init deinit)
    "procedure:{vector:request -> vector:response} false/procedure:{vector:swa-env ->} ... -> (false/procedure ...)
     create a swa-app object that encapsulates the applications respond creation, initialise and deinitialise procedure"
    (list respond (or init identity) (or deinit identity)))

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

  (define (swa-paths-get projects)
    "((symbol ...) ...) -> hashtable:{symbol:project-id-symbol -> string:swa-root}
     convert project names to full paths.
     the current project is included"
    (let (project-ht (ht-make-eq))
      (list
        (map
          (l (id)
            (let*
              ( (relative-path (swa-project-id->relative-path id))
                (found-path
                  (any
                    (l (load-path)
                      (let (path (string-append load-path "/" relative-path))
                        (and (file-exists? path) (ensure-trailing-slash path))))
                    %load-path)))
              (if found-path
                (begin (ht-set! project-ht (swa-project-id->symbol id) found-path) found-path)
                (raise
                  (list (q project-not-in-load-path)
                    "this is required for loading application parts" (q search-paths)
                    %load-path (q projects) projects)))))
          projects)
        project-ht)))

  (define-record swa-env root paths config data)
  (define swa-env-record swa-env)

  (define (swa-start-p projects config proc . arguments)
    "list string/hashtable procedure any ... -> proc-result
     get full paths for project names using the load path, create the swa-env and call proc"
    (list-bind (swa-paths-get projects) (paths project-ht)
      (apply swa-link-root-files paths)
      (apply proc
        (let (root (first paths))
          (record swa-env root
            project-ht
            (if (string? config) (swa-config-get root config) (or config (ht-create-symbol)))
            (ht-create-symbol)))
        arguments)))

  (define-syntax-rules swa-start
    ((((project-id-part ...) ...) a ...) (swa-start-p (q ((project-id-part ...) ...)) a ...))
    (((project-id-part ...) a ...) (swa-start ((project-id-part ...)) a ...))))
