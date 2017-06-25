(library (sph web app client)
  (export
    client-ac-config
    client-css
    client-css-file
    client-delete-compiled-files
    client-file
    client-html
    client-html-file
    client-javascript
    client-javascript-file
    client-port
    shtml-includes-proc
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
     transport protocol agnostic - just writes to port or returns the path of a prepared file")

  ; -- path preparation

  (define (swa-paths-search swa-paths relative-path)
    "(string ...) string -> false/(load-path . relative-path)"
    (any
      (l (load-path)
        (let (path (string-append load-path relative-path))
          (and (file-exists? path) (pair load-path relative-path))))
      swa-paths))

  (define-syntax-rule (client-output-directory swa-root) (string-append swa-root "root/"))
  (define-syntax-rule (client-output-path) "assets/")

  (define-as client-format->suffixes-ht ht-create-symbol
    javascript (list ".sjs" ".js") css (list ".plcss" ".css") html (list ".sxml" ".html"))

  (define-syntax-rule (client-format->suffixes format)
    (ht-ref client-format->suffixes-ht format (list)))

  (define (client-delete-compiled-files swa-root)
    "deletes all filles in the client data output directory with an underscore as prefix,
     which should only be previously generated client-code files"
    (each
      (l (format)
        (let
          (path (string-append (client-output-directory swa-root) (client-output-path) format "/"))
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
    "(string:path-suffix . string:path) symbol -> false/string"
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

  (define (prepare-sources swa-paths sources output-format enter-list?)
    "list symbol boolean -> false/list
     convert source elements of different types to strings.
     * string: relative paths or full path
     * list: recurse once
     * pair: (path-suffix . path) -> string
     * else: identity"
    (and (not (null? sources))
      (every-map
        (l (a)
          (cond
            ( (string? a)
              (if (string-prefix? "/" a) a (path-relative->path-full swa-paths a output-format)))
            ((list? a) (if enter-list? (prepare-sources swa-paths a output-format #f) a))
            ((pair? a) (path-pair->path-full swa-paths a output-format)) (else a)))
        sources)))

  ;-- file processing
  ;
  (define (has-suffix-proc suffix) (l (a) (if (string? a) (string-suffix? suffix a) #t)))
  (define default-env (apply environment (list-q (sph))))
  (define path-uglifyjs (search-env-path "uglifyjs"))

  (define path-csstidy
    ;there is also clean-css, but it does not format and cssbeautify-cli did not work at all
    (search-env-path "csstidy"))

  (define path-html (search-env-path "html"))

  (define (execute-files->port sources port executable . arguments)
    (execute-with-pipes
      (l (in out) (begin-thread (files->port sources in) (close-port in))
        (port-copy-all out port) (close-port out))
      executable arguments #t #t))

  (define javascript-output-compress
    (if path-uglifyjs
      (l (sources port options) "hashtable (string:path ...) port:out -> boolean"
        (execute-files->port sources port path-uglifyjs "--compress" "--mangle"))
      #f))

  (define javascript-output-format
    (if path-uglifyjs
      (l (sources port options) (execute-files->port sources port path-uglifyjs "--beautify")) #f))

  (define css-output-compress
    (if path-csstidy
      (l (sources port options)
        (execute-files->port sources port
          path-csstidy "-" (cli-option "template" "highest") (cli-option "silent" "true")))
      #f))

  (define css-output-format
    (if path-csstidy
      (l (sources port options)
        (l (config sources port)
          (execute-files->port sources port
            path-csstidy "-" (cli-option "template" "default") (cli-option "silent" "true"))))
      #f))

  (define html-output-format
    (if path-html (l (sources port options) (execute-files->port sources port path-html)) #f))

  (define (s-template-sxml->html sources port options)
    "hashtable list port -> string
     compile sxml to xml from s-templates"
    (display "<!doctype html>" port)
    (template-fold (l (template-result . result) (sxml->xml template-result port) result)
      (and options (ht-ref options (q template-bindings)))
      (or (and options (ht-ref options (q template-environment))) default-env) sources))

  (define (s-template-sescript->javascript sources port options)
    "hashtable list port -> string
     compile sescript to javascript from s-templates"
    (let
      (sescript-load-paths
        (or (and options (ht-ref options (q sescript-load-paths))) ses-default-load-paths))
      (template-fold
        (l (template-result . result)
          (sescript->ecmascript template-result port sescript-load-paths) result)
        (and options (ht-ref options (q template-bindings)))
        (or (and options (ht-ref options (q template-environment))) default-env) sources)))

  (define (s-template-plcss->css sources port options)
    "hashtable list port -> string
     compile plcss to css from s-templates"
    (template-fold (l (template-result . result) (plcss->css template-result port) result)
      (and options (ht-ref options (q template-bindings)))
      (or (and options (ht-ref options (q template-environment))) default-env) sources))

  (define-as client-ac-config ht-create-symbol
    ; the main configuration for the asset pipeline
    javascript
    (list
      (ht-create-symbol production javascript-output-compress development javascript-output-format)
      (record ac-config-input (q sescript) (has-suffix-proc ".sjs") s-template-sescript->javascript))
    html
    (list (ht-create-symbol)
      (record ac-config-input "sxml" (has-suffix-proc ".sxml") s-template-sxml->html))
    css
    (list (ht-create-symbol production css-output-compress development css-output-format)
      (record ac-config-input (q plcss) (has-suffix-proc ".plcss") s-template-plcss->css)))

  ;-- main exports

  (define (client-port swa-env port-output output-format bindings sources)
    "port symbol list:alist:template-variables list -> unspecified"
    (and-let* ((sources (prepare-sources (swa-env-paths swa-env) sources output-format #t)))
      (ac-compile client-ac-config (ht-ref (swa-env-config swa-env) (q mode) (q production))
        port-output output-format sources (ht-create-symbol template-bindings bindings))))

  (define (client-file output-format swa-env bindings sources)
    "symbol list:alist:template-variables list -> string:url-path"
    (and-let*
      ( (output-directory (client-output-directory (swa-env-root swa-env)))
        (mode (ht-ref (swa-env-config swa-env) (q mode)))
        (sources (prepare-sources (swa-env-paths swa-env) sources output-format #t))
        (path
          (ac-compile->file client-ac-config mode
            (string-append output-directory (client-output-path)) output-format
            sources #:only-if-newer
            (equal? mode (q production)) #:processor-options
            (ht-create-symbol template-bindings bindings))))
      (string-append "/" (string-drop-prefix output-directory path))))

  (define (client-javascript swa-env port bindings . sources)
    "port list:alist:template-variables string ... -> unspecified
     like client-port with output format \"javascript\""
    (client-port swa-env port (q javascript) bindings sources))

  (define (client-css swa-env port bindings . sources)
    (client-port swa-env port (q css) bindings sources))

  (define (client-html swa-env port bindings . sources)
    (client-port swa-env port (q html) bindings sources))

  (define (client-javascript-file swa-env bindings . sources)
    "list:alist:template-variables string ... -> url-path
     like client-file with output format \"javascript\""
    (client-file (q javascript) swa-env bindings sources))

  (define (client-css-file swa-env bindings . sources)
    (client-file (q css) swa-env bindings sources))

  (define (client-html-file swa-env bindings . sources)
    (client-file (q html) swa-env bindings sources))

  (define (shtml-includes-proc default-sources-css default-sources-javascript)
    "(string ...) (string ...) -> procedure:{symbol procedure:{symbol -> (string ...)} -> list:shtml}
     creates a procedure that returns sxml html for including either css or javascript,
     depending on the format parameter. default-sources-css and default-sources-javascript will be passed to client-file.
     default-sources-css and default-sources-javascript will always be included when the returned procedure is called.
     sources contains additional public relative source file paths or an empty list"
    (let-syntax
      ( (get-sxml
          (syntax-rule (swa-env format sources default-sources client-file create-include-sxml)
            (map create-include-sxml
              (append
                (let (a (apply client-file swa-env #f default-sources)) (if a (any->list a) (list)))
                (if sources (any->list sources) (list)))))))
      (l (swa-env format sources) "vector symbol (string ...) -> list:sxml"
        (if (equal? (q css) format)
          (get-sxml swa-env format sources default-sources-css client-css-file shtml-include-css)
          (get-sxml swa-env format
            sources default-sources-javascript client-javascript-file shtml-include-javascript))))))
