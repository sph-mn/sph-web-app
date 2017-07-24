(library (sph web app client)
  (export
    client-ac-config
    client-css
    client-css-file
    client-delete-compiled-files
    client-file
    client-html
    client-html-file
    client-js
    client-js-file
    client-port
    client-static
    client-static-compile
    client-static-config-create
    client-static-config-create-p
    client-static-p
    sph-web-app-client-description)
  (import
    (ice-9 threads)
    (rnrs eval)
    (sph base)
    (sph filesystem asset-compiler)
    (sph lang plcss)
    (sph lang sescript)
    (sph lang template)
    (sph log)
    (sph process create)
    (sph record)
    (sph web app start)
    (sph web shtml)
    (sxml simple))

  (define sph-web-app-client-description
    "client-code processing
     transport protocol agnostic, it writes to a port or returns the path of a prepared file.
     for the format for sources that can be passed see the documentation of (sph lang template)
     (sph web app client) extends the template-source format to support non-list pairs as arguments:
     example: (string:swa-path-suffix . relative-path)
     the first element selects the swa-path to use based on its suffix.
     source-path template: {swa-path}/client/{output-format}/{path-suffix}
     destination-path template: {swa-root}/root/assets/{output-format}/_{sources-dependent-name}
     # syntax
     client-static-config-create :: symbol/(symbol ...) (output-format symbol:bundle-id/(list:template-bindings any:source ...) ...) ...
       creates a configuration object for client-static-compile.
       all arguments are literals and output-format configurations are quasiquoted, so unquote can be used")

  ; -- path preparation

  (define (swa-paths-search swa-paths relative-path)
    "(string ...) string -> false/(load-path . relative-path)"
    (any
      (l (load-path)
        (let (path (string-append load-path relative-path))
          (and (file-exists? path) (pair load-path relative-path))))
      swa-paths))

  (define-syntax-rule (client-output-directory swa-root) (string-append swa-root "root/"))
  (define client-output-path "assets/")

  (define-as client-format->suffixes-ht ht-create-symbol
    js (list ".sjs" ".js") css (list ".plcss" ".css") html (list ".sxml" ".html"))

  (define-syntax-rule (client-format->suffixes format)
    (ht-ref client-format->suffixes-ht format (list)))

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

  (define (path-relative->path-full swa-paths path format)
    (let*
      ( (path (path-add-prefix path format))
        (path
          (or (swa-paths-search swa-paths path)
            (any (l (suffix) (swa-paths-search swa-paths (string-append path suffix)))
              (client-format->suffixes format))
            (begin (log-message (q error) (string-append "missing file \"" path "\"")) #f))))
      (if path (string-append (first path) "/" (tail path)))))

  (define (path-pair->path-full swa-paths a format)
    "(string:path-suffix . string:path) symbol -> false/string
     suffix is of a swa-path"
    (let*
      ( (path-suffix (ensure-trailing-slash (first a)))
        (path-full
          (any
            (l (b)
              (and (string-suffix? path-suffix b)
                (string-append b (path-add-prefix (tail a) format))))
            swa-paths)))
      (if (and path-full (file-exists? path-full)) path-full
        (begin (log-message (q error) (string-append "missing file \"" path-full "\"")) #f))))

  (define* (prepare-sources swa-paths sources output-format #:optional (depth 0))
    "(string ...) list symbol [integer] -> any
     normalise the (sph web app client) sources format to the (sph lang template) template-fold sources format.
     first level:
     * string: relative paths or full path
     * list: recurse once
     first/second level:
     * pair: (path-suffix . path) -> string
     * else: identity"
    (if (list? sources)
      (every-map
        (l (a)
          (cond
            ( (and (string? a) (= 0 depth))
              (if (string-prefix? "/" a) a (path-relative->path-full swa-paths a output-format)))
            ((list? a) (if (= 0 depth) (prepare-sources swa-paths a output-format (+ 1 depth)) a))
            ((pair? a) (path-pair->path-full swa-paths a output-format)) (else a)))
        sources)
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
    (display "<!doctype html>" port)
    (template-fold (l (template-result . result) (sxml->xml template-result port) result)
      (and options (ht-ref options (q template-bindings)))
      (or (and options (ht-ref options (q template-environment))) default-env) source))

  (define (s-template-sescript->js sources port options)
    "hashtable list port -> string
     compile sescript to js from s-templates"
    (let
      (sescript-load-paths
        (or (and options (ht-ref-q options sescript-load-paths)) ses-default-load-paths))
      (template-fold
        (l (template-result . result)
          (sescript->ecmascript template-result port sescript-load-paths) result)
        (and options (ht-ref-q options template-bindings))
        (or (and options (ht-ref-q options template-environment)) default-env) sources)))

  (define (s-template-plcss->css source port options)
    "hashtable list port -> string
     compile plcss to css from s-templates"
    (template-fold (l (template-result . result) (plcss->css template-result port) result)
      (and options (ht-ref-q options template-bindings))
      (or (and options (ht-ref-q options template-environment)) default-env) source))

  (define (string-and-suffix-proc suffix) (l (a) (and (string? a) (string-suffix? suffix a))))

  (define (string-and-suffix-or-true-proc suffix)
    (l (a) (if (string? a) (string-suffix? suffix a) #t)))

  (define (development-mode? options) (eq? (q development) (ht-ref-q options mode)))

  (define html-output-processor
    (let (path-html (search-env-path* "html"))
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
                  (l (child-in child-out)
                    (begin-thread (process-input child-in) (close-port child-in))
                    (port-copy-all child-out out))
                  path-uglifyjs (list "--beautify") #t #t)))
            (compress
              (l (process-input out options)
                (execute-with-pipes
                  (l (child-in child-out)
                    (begin-thread (process-input child-in) (close-port child-in))
                    (port-copy-all child-out out))
                  path-uglifyjs (list "--compress" "--mangle") #t #t))))
          (l (process-input out-port options)
            ((if (development-mode? options) format compress) process-input out-port options))))))

  (define (ac-input-copy source out options) (file->port source out))

  (define-as client-ac-config ht-create-symbol
    ; the main configuration for the asset pipeline
    html
    (list html-output-processor (vector (q html) (string-and-suffix-proc ".html") ac-input-copy)
      (vector (q sxml) (string-and-suffix-or-true-proc ".sxml") s-template-sxml->html))
    css
    (list css-output-processor (vector (q css) (string-and-suffix-proc ".css") ac-input-copy)
      (vector (q plcss) (string-and-suffix-or-true-proc ".plcss") s-template-plcss->css))
    js
    (list js-output-processor (vector (q js) (string-and-suffix-proc ".js") ac-input-copy)
      (vector (q sescript) (string-and-suffix-or-true-proc ".sjs") s-template-sescript->js)))

  ;-- main exports

  (define (bindings-add-swa-env swa-env bindings)
    (pair (pair (q swa-env) swa-env) (or bindings (list))))

  (define (client-port output-format port-output swa-env bindings sources)
    "port symbol list:alist:template-variables list -> unspecified"
    (and-let* ((sources (prepare-sources (swa-env-paths swa-env) sources output-format)))
      (ac-compile client-ac-config output-format
        sources port-output
        (ht-create-symbol mode (ht-ref (swa-env-config swa-env) (q mode) (q production))
          template-bindings (bindings-add-swa-env swa-env bindings)))))

  (define (client-file output-format swa-env bindings sources)
    "symbol list:alist:template-variables list -> string:url-path
     sources are given in (sph lang template) template-fold source format"
    (and-let*
      ( (output-directory (client-output-directory (swa-env-root swa-env)))
        (config (swa-env-config swa-env)) (mode (ht-ref-q config mode (q production)))
        (when (ht-ref-q config client-file-when (if (eq? (q development) mode) (q always) (q new))))
        (sources (prepare-sources (swa-env-paths swa-env) sources output-format))
        (path
          (ac-compile->file client-ac-config output-format
            sources (string-append output-directory client-output-path)
            #:when when
            #:processor-options
            (ht-create-symbol template-bindings (bindings-add-swa-env swa-env bindings)))))
      (string-append "/" (string-drop-prefix output-directory path))))

  (define (client-js swa-env port bindings . sources)
    "port list:alist:template-variables string ... -> unspecified
     like client-port with output format \"js\""
    (client-port (q js) port swa-env bindings sources))

  (define (client-css swa-env port bindings . sources)
    (client-port (q css) port swa-env bindings sources))

  (define (client-html swa-env port bindings . sources)
    (client-port (q html) port swa-env bindings sources))

  (define (client-js-file swa-env bindings . sources)
    "list:alist:template-variables string ... -> url-path
     like client-file with output format \"js\""
    (client-file (q js) swa-env bindings sources))

  (define (client-css-file swa-env bindings . sources)
    (client-file (q css) swa-env bindings sources))

  (define (client-html-file swa-env bindings . sources)
    (client-file (q html) swa-env bindings sources))

  ;-- pre-compilation with configuration object

  (define-syntax-case (project-id->symbol a) s
    ; symbol/(symbol ...) -> symbol
    ; if project-id is not a symbol but a list with symbols, join the list elements separated by "-" to create a new symbol.
    ; this is done to be able to use eq? with the resulting identifier, list literals did not work
    (let (b (syntax->datum (syntax a)))
      (if (symbol? b) (syntax (quote a))
        (datum->syntax s (list (q quote) (string->symbol (string-join (map symbol->string b) "-")))))))

  (define-syntax-rule
    (client-static-config-create project-id (output-format bundle-id/sources ...) ...)
    (client-static-config-create-p (project-id->symbol project-id)
      (qq ((output-format bundle-id/sources ...) ...))))

  (define-syntax-rule (client-static swa-env project-id format bundle-ids)
    (client-static-p swa-env (project-id->symbol project-id) format bundle-ids))

  (define (client-static-config-create-p project-id data) "symbol list -> list"
    (pair project-id
      (map
        (l (a) "(output-format key/sources ...) -> unspecified"
          (pair (first a) (list->alist (tail a))))
        data)))

  (define (client-static-compile swa-env config)
    "vector list -> unspecified
     pre-compile the static client files to be served and save result file paths in swa-data.
     creates a nested hashtable structure in swa-data with the following hierarchy:
     (client-static project-id output-format bundle-id compiled-source-path)"
    (let
      ( (data-ht
          (ht-ref (swa-env-data swa-env) (q client-static)
            (let (a (ht-make-eq)) (ht-set! (swa-env-data swa-env) (q client-static) a) a)))
        (project-id (first config)) (format-config (tail config)) (project-ht (ht-make-eq)))
      (ht-set! data-ht project-id project-ht)
      (each
        (l (a)
          (let ((output-format (first a)) (bundle-config (tail a)) (format-ht (ht-make-eq)))
            (ht-set! project-ht output-format format-ht)
            (each
              (l (a)
                (let ((key (first a)) (bindings-and-sources (tail a)))
                  (ht-set! format-ht key
                    (client-file output-format swa-env
                      (first bindings-and-sources) (tail bindings-and-sources)))))
              bundle-config)))
        format-config)))

  (define (client-static-p swa-env project-id format bundle-ids)
    "vector symbol symbol symbol -> false/string
     get compiled source paths for bundle-ids"
    (let (format-ht (ht-tree-ref (swa-env-data swa-env) (q client-static) project-id format))
      (map (l (a) (ht-ref format-ht a)) bundle-ids))))
