(library (sph web app client)
  (export
    client-ac-config
    client-css
    client-delete-compiled-files
    client-file
    client-file-css
    client-file-html
    client-file-js
    client-html
    client-js
    client-port
    client-static
    client-static-compile
    client-static-config-create
    client-static-config-create-p
    sph-web-app-client-description)
  (import
    (ice-9 threads)
    (rnrs eval)
    (sph)
    (sph alist)
    (sph filesystem)
    (sph filesystem asset-compiler)
    (sph hashtable)
    (sph io)
    (sph lang plcss)
    (sph lang sescript)
    (sph lang template)
    (sph list)
    (sph log)
    (sph one)
    (sph process create)
    (sph record)
    (sph string)
    (sph web app start)
    (sph web shtml)
    (sxml simple))

  (define sph-web-app-client-description
    "client-code processing. create target language files from templates and source files of varying formats with pre-processing.
     sources
       the data structure given as sources depends on the input processor for the associated input format.
       see client-ac-config, client-input-id->suffixes-ht and (sph filesystem asset-compiler) for how to add new asset processors
     s-template formats
       the default sxml, plcss and sescript input processor is configured to use (sph lang template) and its template-fold source format with template bindings.
       the template-fold source format is resolves relative paths relative to a web-app project root.
       the source path is constructed with this template: {swa-path}/client/{output-format}/{requested-path}
       filename extensions are optional.
       non-list pairs are supported as arguments
         example: (symbol/(symbol...):project-id . relative-path)
         this is for selecting files from other web-app projects
     syntax
       client-static-config-create :: symbol/(symbol ...) (output-format-id symbol:bundle-id/(list:template-bindings any:source ...) ...) ...
         creates a configuration object for client-static-compile.
         all arguments are literals and output-format configurations are quasiquoted, so unquote can be used
         example:
         (define client-static-config
           (client-static-config-create project-name
             (bundle-name
               css (#f \"testfile\")
               js ((unquote alist-q testvariable \"testvalue\") \"testsjsfile\"))
             (another-bundle
               css (#f \"otherfile\"))))")

  ; -- path preparation
  (define client-output-path "assets/")
  (define-syntax-rule (client-output-directory swa-root) (string-append swa-root "root/"))

  (define-syntax-rule (client-input-id->suffixes format)
    (ht-ref client-input-suffixes-ht format (list)))

  (define (swa-paths-search swa-paths relative-path)
    "(string ...) string -> false/(load-path . relative-path)"
    (any
      (l (load-path)
        (let (path (string-append load-path relative-path))
          (and (file-exists? path) (pair load-path relative-path))))
      swa-paths))

  (define-as client-input-suffixes-ht ht-create-symbol-q
    ; for the optional suffix in source filenames
    js (list ".sjs" ".js") css (list ".plcss" ".css") html (list ".sxml" ".html"))

  (define (client-delete-compiled-files swa-root)
    "deletes all filles in the client data output directory with an underscore as prefix,
     which should only be previously generated client-code files"
    (each
      (l (format)
        (let
          (path (string-append (client-output-directory swa-root) client-output-path format "/"))
          (if (file-exists? path)
            (directory-fold path
              (l (e result) (if (string-prefix? "_" e) (delete-file (string-append path e)))) #f))))
      (map symbol->string (vector->list (ht-keys client-ac-config)))))

  (define (path-add-prefix a format) "string symbol -> string"
    (string-append "client/" (symbol->string format) "/" a))

  (define (path-find-with-suffix a format-suffixes)
    (or (and (file-exists? a) a)
      (any (l (suffix) (let (b (string-append a suffix)) (and (file-exists? b) b))) format-suffixes)))

  (define (log-missing-file a) (log-message (q error) (string-append "missing file \"" a "\"")) #f)

  (define (path-relative->path-full swa-root path format format-suffixes)
    (let*
      ( (path (string-append swa-root (path-add-prefix path format)))
        (found-path (path-find-with-suffix path format-suffixes)))
      (if found-path found-path (log-missing-file path))))

  (define (path-pair->path-full swa-paths a format format-suffixes)
    "(string:path-suffix . string:path) symbol -> false/string
     suffix is of a swa-path"
    (path-relative->path-full (ht-ref swa-paths (first a)) (tail a) format format-suffixes))

  (define (prepare-sources default-project swa-paths sources output-format)
    "(string ...) list symbol [integer] -> any
     normalise the (sph web app client) sources format to the (sph lang template) template-fold sources format.
     allow usage of filenames without filename extension.
     first level:
     * string: relative paths or full path
     * list: recurse once
     first/second level:
     * pair: (project-id . path) -> string
     * else: identity"
    (if (list? sources)
      (let
        ( (default-root (ht-ref swa-paths (swa-project-id->symbol* default-project)))
          (format-suffixes (client-input-id->suffixes output-format)))
        (let loop ((sources sources) (depth 0))
          (every-map
            (l (a)
              (cond
                ( (and (string? a) (= 0 depth))
                  (if (string-prefix? "/" a) a
                    (path-relative->path-full default-root a output-format format-suffixes)))
                ((list? a) (if (= 0 depth) (loop a (+ 1 depth)) a))
                ((pair? a) (path-pair->path-full swa-paths a output-format format-suffixes))
                (else a)))
            sources)))
      sources))

  ;-- file processing
  ;
  (define default-env (apply environment (list-q (sph))))
  (define (search-env-path* a) (first-or-false (search-env-path (list a))))

  (define (execute-files->port sources port executable . arguments)
    "(string ...) port string string ... ->
     run executable and write file content from sources to it and copy the result to port"
    (execute-with-pipes
      (l (in out) (begin-thread (files->port sources in) (close-port in))
        (port-copy-all out port) (close-port out))
      executable arguments #t #t))

  (define (s-template-sxml->html source port options)
    "hashtable list port -> string
     compile sxml to xml from s-templates"
    (put-string port "<!doctype html>")
    (template-fold
      (l (template-result . result) (sxml->xml template-result port) (newline port) result)
      (and options (ht-ref options (q template-bindings)))
      (or (and options (ht-ref options (q template-environment))) default-env) source))

  (define (s-template-sescript->js sources port options)
    "hashtable list port -> string
     compile sescript to js from s-templates"
    (let
      (sescript-load-paths
        (or (and options (ht-ref-q options sescript-load-paths)) ses-default-load-paths))
      (template-fold
        (l (template-result . result) (sescript-use-strict port)
          (sescript->ecmascript template-result port sescript-load-paths) (newline port) result)
        (and options (ht-ref-q options template-bindings))
        (or (and options (ht-ref-q options template-environment)) default-env) sources)))

  (define (s-template-plcss->css source port options)
    "hashtable list port -> string
     compile plcss to css from s-templates"
    (template-fold
      (l (template-result . result) (plcss->css template-result port) (newline port) result)
      (and options (ht-ref-q options template-bindings))
      (or (and options (ht-ref-q options template-environment)) default-env) source))

  (define (development-mode? options) (eq? (q development) (ht-ref-q options mode)))

  (define html-output-processor
    ; disabled, because it can change how things are rendered
    (let (path-html (and #f (search-env-path* "html")))
      (and path-html
        (l (process-input out options)
          (if (development-mode? options)
            (execute-with-pipes
              (l (child-in child-out) (begin-thread (process-input child-in) (close-port child-in))
                (port-copy-all child-out out))
              path-html (list) #t #t)
            (process-input out))))))

  (define css-output-processor
    ; compress of format css depending on the option "mode"
    (let
      (path-csstidy
        ; clean-css does not format and cssbeautify-cli did not work at all
        (search-env-path* "csstidy"))
      (and path-csstidy
        (let
          ( (format
              (l (process-input out options)
                (execute-with-pipes
                  (l (child-in child-out)
                    (begin-thread (process-input child-in) (close-port child-in))
                    (port-copy-all child-out out))
                  path-csstidy
                  (list "-" (cli-option "template" "default") (cli-option "silent" "true")) #t #t)))
            (compress
              (l (process-input out options)
                (execute-with-pipes
                  (l (child-in child-out)
                    (begin-thread (process-input child-in) (close-port child-in))
                    (port-copy-all child-out out))
                  path-csstidy
                  (list "-" (cli-option "template" "highest") (cli-option "silent" "true")) #t #t))))
          (l (process-input out-port options)
            ((if (development-mode? options) format compress) process-input out-port options))))))

  (define js-output-processor
    ; compress of format js code depending on the option "mode"
    (let (path-uglifyjs (search-env-path* "uglifyjs"))
      (and path-uglifyjs
        (let
          ( (format
              (l (process-input out options)
                (execute-with-pipes
                  (l (child-in child-out child-err)
                    (begin-thread (process-input child-in) (close-port child-in))
                    (begin-thread (port-copy-all child-err (current-error-port))
                      (close-port child-err))
                    (port-copy-all child-out out))
                  path-uglifyjs (list "--beautify") #t #t #t)))
            (compress
              (l (process-input out options)
                (execute-with-pipes
                  (l (child-in child-out child-err)
                    (begin-thread (process-input child-in) (close-port child-in))
                    (begin-thread (port-copy-all child-err (current-error-port))
                      (close-port child-err))
                    (port-copy-all child-out out))
                  path-uglifyjs (list "--compress" "--mangle") #t #t #t))))
          (l (process-input out-port options)
            ((if (development-mode? options) format compress) process-input out-port options))))))

  (define (match-template-source-f suffix) (l (a) (if (string? a) (string-suffix? suffix a) #t)))

  (define-as client-ac-config ac-config
    ; the main configuration for the supported formats of the asset pipeline.
    ; see (sph filesystem asset-compiler)
    ( (html #t html-output-processor) (html #t #f)
      (sxml (match-template-source-f ".sxml") s-template-sxml->html))
    ( (css #t css-output-processor) (css #t #f)
      (plcss (match-template-source-f ".plcss") s-template-plcss->css))
    ( (js #t js-output-processor) (js #t #f)
      (sjs (match-template-source-f ".sjs") s-template-sescript->js)))

  ;-- main exports

  (define (bindings-add-swa-env swa-env bindings)
    (pair (pair (q swa-env) swa-env) (or bindings (list))))

  (define (client-port swa-env output-format port-output bindings default-project-id sources)
    "port symbol list:alist:template-variables list -> unspecified"
    (and-let*
      ((sources (prepare-sources default-project-id (swa-env-paths swa-env) sources output-format)))
      (ac-compile client-ac-config output-format
        sources port-output
        (ht-create-symbol-q mode (ht-ref (swa-env-config swa-env) (q mode) (q production))
          template-bindings (bindings-add-swa-env swa-env bindings)))))

  (define*
    (client-file swa-env output-format bindings default-project-id sources #:optional file-name)
    "symbol false/list:alist:template-variables list -> string:url-path
     destination-path template: {swa-root}/root/assets/{output-format}/_{sources-dependent-name}"
    (and-let*
      ( (output-directory (client-output-directory (swa-env-root swa-env)))
        (config (swa-env-config swa-env)) (mode (ht-ref-q config mode (q production)))
        (when
          (or (ht-ref-q config client-file-when) (if (eq? (q development) mode) (q always) (q new))))
        (sources (prepare-sources default-project-id (swa-env-paths swa-env) sources output-format))
        (path
          (ac-compile->file client-ac-config output-format
            sources (string-append output-directory client-output-path)
            #:when when
            #:processor-options
            (ht-create-symbol-q mode mode template-bindings (bindings-add-swa-env swa-env bindings))
            #:dest-name file-name)))
      (string-append "/" (string-drop-prefix output-directory path))))

  (define (client-js swa-env port bindings project . sources)
    "port list:alist:template-variables string ... -> unspecified
     like client-port with output format \"js\""
    (client-port swa-env (q js) port bindings project sources))

  (define (client-css swa-env port bindings project . sources)
    (client-port swa-env (q css) port bindings project sources))

  (define (client-html swa-env port bindings project . sources)
    (client-port swa-env (q html) port bindings project sources))

  (define (client-file-js swa-env bindings project . sources)
    "list:alist:template-variables string ... -> url-path
     like client-file with output format \"js\""
    (client-file swa-env (q js) bindings project sources))

  (define (client-file-css swa-env bindings project . sources)
    (client-file swa-env (q css) bindings project sources))

  (define (client-file-html swa-env bindings project . sources)
    (client-file swa-env (q html) bindings project sources))

  ;-- pre-compilation with configuration object
  ;
  (define (swa-project-id->symbol* a) (if (symbol? a) a (swa-project-id->symbol a)))

  (define-syntax-rules client-static-config-create
    ( ( (project-id-part ...) (output-format bundle-id/sources ...) ...)
      (client-static-config-create-p (q (project-id-part ...))
        (qq ((output-format bundle-id/sources ...) ...))))
    ((project-id a ...) (client-static-config-create (project-id) a ...)))

  (define (client-static-config-create-p project-id data) "symbol list -> list"
    (pair project-id
      (map
        (l (a) "(bundle-id format/sources ...) -> unspecified"
          (pair (first a) (list->alist (tail a))))
        data)))

  (define (client-static-compile swa-env config)
    "vector list -> unspecified
     pre-compile the static client files to be served and save result file paths in swa-data.
     creates a nested hashtable structure in swa-data with the following hierarchy:
     client-static > project-id-symbol > bundle-id > output-format > compiled-source-path"
    (let*
      ( (data-ht
          (or (ht-ref (swa-env-data swa-env) (q client-static))
            (let (a (ht-create-symbol)) (ht-set! (swa-env-data swa-env) (q client-static) a) a)))
        (project-id (first config)) (bundles (tail config))
        (project-symbol (swa-project-id->symbol* project-id))
        (project-ht (or (ht-ref data-ht project-symbol) (ht-create-symbol))))
      (ht-set! data-ht project-symbol project-ht)
      (each
        (l (a)
          (let ((bundle (first a)) (format-and-source (tail a)) (format-ht (ht-make-eq)))
            (ht-set! project-ht bundle format-ht)
            (each
              (l (a)
                (let ((format (first a)) (bindings-and-sources (tail a)))
                  (ht-set! format-ht format
                    (client-file swa-env format
                      (first bindings-and-sources) project-id
                      (tail bindings-and-sources)
                      (string-append "_" (symbol->string project-symbol)
                        "_" (symbol->string bundle))))))
              format-and-source)))
        bundles)))

  (define (client-static swa-env project-id format bundle-ids)
    "vector symbol symbol symbol -> false/string
     get compiled source paths for bundle-ids"
    (let
      (bundle-ht
        (ht-tree-ref (swa-env-data swa-env) (q client-static) (swa-project-id->symbol* project-id)))
      (map (l (a) (ht-ref (ht-ref bundle-ht a) format)) bundle-ids))))
