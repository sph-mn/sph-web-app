(library (sph web app start)
  (export
    sph-web-app-start-description
    swa-config-get
    swa-create
    swa-env-config
    swa-env-config-set!
    swa-env-data
    swa-env-data-set!
    swa-env-paths
    swa-env-record
    swa-env-root
    swa-project-id->symbol
    swa-start
    swa-start-p)
  (import
    (rnrs exceptions)
    (sph)
    (sph filesystem)
    (sph hashtable)
    (sph lang config)
    (sph list)
    (sph record)
    (only (guile)
      string-join
      file-exists?
      dirname
      string-suffix?
      %load-path)
    (only (sph process) shell-eval)
    (only (sph string) string-equal?))

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

  (define (swa-project-id->relative-path a)
    (string-append (string-join (map symbol->string a) "/") ".scm"))

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

  (define (swa-config-get-file swa-root name)
    "string string/hashtable/false -> list
     get a hashtable for the configuration file identified by \"name\".
     if a configuration file with the name default.scm exists, it is loaded first and
     unless name is default, the configuration file identified by name overwrites values
     in the default config.
     for names other than \"default\" a configuration file must exist"
    (let*
      ( (default-path (string-append swa-root "config/default.scm"))
        (config
          (or (and (file-exists? default-path) (config-read-file default-path)) (ht-create-symbol)))
        (name-config
          (and (not (string-equal? "default" name))
            (config-read-file (string-append swa-root "config/" name ".scm")))))
      (if name-config (ht-tree-merge! config name-config)) config))

  (define (swa-config-get swa-root config)
    (if (string? config) (swa-config-get-file swa-root config) (or config (ht-create-symbol))))

  (define (swa-paths-get projects load-paths)
    "((symbol ...) ...) (string ...) -> list hashtable:{symbol:project-id-symbol -> string:swa-root}
     get project root directories from project/module names. the current project is the first element.
     also create a hashtable that maps project names to project root paths"
    (let ((project-ht (ht-make-eq)))
      (list
        (map
          (l (id)
            (let*
              ( (relative-path (swa-project-id->relative-path id))
                (root
                  (any
                    (l (load-path)
                      (let (path (string-append load-path "/" relative-path))
                        (and (file-exists? path) (ensure-trailing-slash (dirname load-path)))))
                    load-paths)))
              (if root (begin (ht-set! project-ht (swa-project-id->symbol id) root) root)
                (raise
                  (list (q project-not-found) "project not found in any module load path"
                    (q load-paths) load-paths (q projects) projects)))))
          projects)
        project-ht)))

  (define-record swa-env root paths config data)
  (define swa-env-record swa-env)

  (define (swa-start projects config proc . arguments)
    "(symbol ...)/((symbol ...) ...):module-names string/hashtable/false procedure any ... -> proc-result
     create the swa-env object and call proc. the app is to be initialised in proc.
     gets full paths for project names using current module load paths.
     also links files from included root/ directories into the parent project"
    (let
      ( (projects (or (and (symbol? (first projects)) (list projects)) projects))
        (load-paths (filter (l (a) (string-suffix? "modules" a)) %load-path)))
      (list-bind (swa-paths-get projects load-paths) (paths project-ht)
        (apply swa-link-root-files paths)
        (apply proc
          (let (root (first paths))
            (record swa-env root project-ht (swa-config-get root config) (ht-create-symbol)))
          arguments)))))
