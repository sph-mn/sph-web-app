(define-module (sph web app client))

(use-modules (ice-9 threads) (srfi srfi-2)
  (srfi srfi-1) (rnrs eval)
  (sph) (sph alist)
  ((rnrs io ports) #:select (put-string)) (sph filesystem)
  (sph filesystem asset-compiler) (sph hashtable)
  (sph io) (sph lang plcss)
  (sph lang sescript) (sph list)
  (sph log) (sph other)
  (sph process create) (sph string) (sph web app base) (sph web shtml) (sxml simple))

(export client-ac-config client-delete-compiled-files
  client-file client-port
  client-static client-static-compile
  client-static-config-create client-static-config-create-p sph-web-app-client-description)

(define sph-web-app-client-description
  "client-code processing.
   depends on sescript.
   # features
   * read files from a swa-root relative path
   * write files to a swa-root relative path
   * preprocess or copy files. by default support for css, html, js, plcss, shtml, sescript
   * by default create output file names automatically
   * optionally ignore files if already existing and unchanged
   * template variables for s-expression based input formats, accessible with unquote
   * compile bundles of possibly preprocessed files and get the public path by bundle id (client-static)
   * source paths are constructed corresponding to this template by default: {swa-root}/client/{output-format}/{requested-path}
   * source paths do not need to have filename extensions
   * create compiled files whose paths are later accesible by bundle-ids
   # client-static-config-create example
   (client-static-config-create
     (default
       css (((template-variable-name . (unquote 123))) \"sph\" \"sph-cms\")
       js (\"foreign/crel\" \"foreign/underscore\"))
     (c-view
       css (\"content/view\")
       js (\"content/view\")))")

(define client-output-path "assets/")

(define (client-output-directory swa-env)
  (string-append (swa-env-root swa-env)
    (ensure-trailing-slash (or (ht-ref-q (swa-env-config swa-env) client-output-path) "webroot/"))))

(define (file->list read path) (call-with-input-file path (l (port) (port->list read port))))
(define (log-missing-file a) (log-message (q error) (string-append "missing file \"" a "\"")) #f)
(define (search-env-path* a) (first-or-false (search-env-path (list a))))
(define (options-development-mode? options) (eq? (q development) (ht-ref-q options mode)))
(define template-default-env (environment (q (rnrs base)) (q (sph)) (q (sph web app base))))

(define client-format-suffixes
  (ht-create-symbol-q js (list ".sjs" ".js")
    css (list ".plcss" ".css") html (list ".shtml" ".html")))

(define (port->list read port)
  (let loop ((a (read port))) (if (eof-object? a) (list) (pair a (loop (read port))))))

(define (template-get source env)
  "string/port/any -> procedure:{bindings:alist -> any}
   returns a procedures that quasiquotes s-expressions read from file or passed and evaluates
   the quasiquote with given template variables available in unquote"
  (let
    (data
      (if (string? source) (file->list read source)
        (if (port? source) (port->list read source) source)))
    (if data
      (l (bindings) "alist -> any"
        (apply
          (eval (qq (lambda (unquote (map first bindings)) (unquote (list (q quasiquote) data))))
            env)
          (map tail bindings)))
      (throw (q template-not-found) source))))

(define (client-delete-compiled-files swa-env)
  "deletes all files in the client data output directory with an underscore as prefix,
   which should only be previously generated client-code files"
  (let (format-name-strings (map symbol->string (vector->list (ht-keys client-ac-config))))
    (each
      (l (format)
        (let (path (string-append (client-output-directory swa-env) client-output-path format "/"))
          (if (file-exists? path)
            (each delete-file (directory-list-full path (l (a) (string-prefix? "_" a)))))))
      format-name-strings)))

(define (client-source-full-path swa-env path format format-suffixes)
  (if (string-prefix? "/" path) path
    (let*
      ( (path (string-append (swa-env-root swa-env) "client/" (symbol->string format) "/" path))
        (found-path
          (if (file-exists? path) path
            (any (l (suffix) (let (b (string-append path suffix)) (and (file-exists? b) b)))
              format-suffixes))))
      (or found-path (log-missing-file path)))))

(define (prepare-sources swa-env sources output-format)
  "any list symbol [integer] -> any
   allow to source file paths without filename suffix"
  (let (format-suffixes (ht-ref client-format-suffixes output-format null))
    (filter-map
      (l (a) (if (string? a) (client-source-full-path swa-env a output-format format-suffixes) a))
      sources)))

(define (template-sxml->html source port options) "string/sxml port hashtable -> string"
  (let
    (template-result
      ( (template-get source (ht-ref options (q template-environment)))
        (ht-ref options (q template-bindings))))
    (put-string port "<!doctype html>") (sxml->xml template-result port) (newline port)))

(define (template-sescript->js source port options) "string/sescript port hashtable -> string"
  (let
    ( (sescript-load-paths (ht-ref-q options sescript-load-paths))
      (template-result
        ( (template-get source (ht-ref options (q template-environment)))
          (ht-ref options (q template-bindings)))))
    (sescript-use-strict port) (sescript->ecmascript template-result port sescript-load-paths)
    (newline port)))

(define (template-plcss->css source port options)
  (let
    (template-result
      ( (template-get source (ht-ref options (q template-environment)))
        (ht-ref options (q template-bindings))))
    (plcss->css template-result port) (newline port)))

(define css-output-processor
  (and-let* ((path-csstidy (search-env-path* "csstidy")))
    (let
      ( (format
          (l (write-input out options)
            (execute-with-pipes
              (l (child-in child-out) (begin-thread (write-input child-in) (close-port child-in))
                (port-copy-all child-out out))
              path-csstidy (list "-" (cli-option "template" "default") (cli-option "silent" "true"))
              #t #t)))
        (compress
          (l (write-input out options)
            (execute-with-pipes
              (l (child-in child-out) (begin-thread (write-input child-in) (close-port child-in))
                (port-copy-all child-out out))
              path-csstidy (list "-" (cli-option "template" "highest") (cli-option "silent" "true"))
              #t #t))))
      (l (write-input out-port options)
        ((if (options-development-mode? options) format compress) write-input out-port options)))))

(define js-output-processor
  (and-let* ((path-uglifyjs (search-env-path* "uglifyjs")))
    (let
      ( (format
          (l (write-input out options)
            (execute-with-pipes
              (l (child-in child-out child-err)
                (begin-thread (write-input child-in) (close-port child-in))
                (begin-thread (port-copy-all child-err (current-error-port)) (close-port child-err))
                (port-copy-all child-out out))
              path-uglifyjs (list "--beautify") #t #t #t)))
        (compress
          (l (write-input out options)
            (execute-with-pipes
              (l (child-in child-out child-err)
                (begin-thread (write-input child-in) (close-port child-in))
                (begin-thread (port-copy-all child-err (current-error-port)) (close-port child-err))
                (port-copy-all child-out out))
              path-uglifyjs (list "--compress" "--mangle") #t #t #t))))
      (l (write-input out-port options)
        ((if (options-development-mode? options) format compress) write-input out-port options)))))

(define client-ac-config
  (ac-config-new
    (list
      (list (q html) (ac-config-output (q html) ".html" ac-output-copy)
        (ac-config-input (q html) ".html" ac-input-copy)
        (ac-config-input (q shtml) ".shtml" template-sxml->html)
        (ac-config-input (q shtml-data) (const #t) template-sxml->html))
      (list (q css) (ac-config-output (q css) ".css" (or css-output-processor ac-output-copy))
        (ac-config-input (q css) ".css" ac-input-copy)
        (ac-config-input (q plcss) ".plcss" template-plcss->css)
        (ac-config-input (q plcss-data) (const #t) template-plcss->css))
      (list (q js) (ac-config-output (q js) ".js" (or js-output-processor ac-output-copy))
        (ac-config-input (q js) ".js" ac-input-copy)
        (ac-config-input (q sjs) ".sjs" template-sescript->js)
        (ac-config-input (q sjs-data) (const #t) template-sescript->js)))))

(define (bindings-add-swa-env swa-env bindings)
  (pair (pair (q swa-env) swa-env) (or bindings (list))))

(define (client-port swa-env output-format port-output bindings sources)
  "port symbol port ((symbol:name . any:value) ...):template-variables list -> unspecified"
  (and-let* ((sources (prepare-sources swa-env sources output-format)))
    (ac-compile client-ac-config output-format
      sources port-output
      (ht-create-symbol-q template-environment
        (ht-ref (swa-env-config swa-env) (q template-environment) template-default-env) mode
        (ht-ref (swa-env-config swa-env) (q mode) (q production)) template-bindings
        (bindings-add-swa-env swa-env bindings)))))

(define* (client-file swa-env output-format bindings sources #:optional file-name)
  "vector symbol false/list:alist:template-variables list [string] -> string:url-path
   default destination-path template: {swa-root}/webroot/assets/{output-format}/_{sources-dependent-name}"
  (and-let*
    ( (output-directory (client-output-directory swa-env)) (config (swa-env-config swa-env))
      (mode (ht-ref-q config mode (q production)))
      (web-base-path (ht-ref-q config web-base-path "/"))
      (when
        (or (ht-ref-q config client-file-when) (if (eq? (q development) mode) (q always) (q new))))
      (sources (prepare-sources swa-env sources output-format))
      (path
        (ac-compile->file client-ac-config output-format
          sources (string-append output-directory client-output-path)
          #:when when
          #:processor-options
          (ht-create-symbol-q template-environment
            (ht-ref (swa-env-config swa-env) (q template-environment) template-default-env) mode
            mode template-bindings (bindings-add-swa-env swa-env bindings))
          #:file-name file-name)))
    (string-append web-base-path (string-drop-prefix output-directory path))))

(define-syntax-rule (client-static-config-create (bundle-id format/sources ...) ...)
  (client-static-config-create-p (qq ((bundle-id format/sources ...) ...))))

(define (client-static-config-create-p data) "list -> list"
  (map
    (l (a) "(bundle-id format/sources ...) -> unspecified" (pair (first a) (list->alist (tail a))))
    data))

(define (client-static-compile swa-env config)
  "vector list -> unspecified
   pre-compile the static client files to be served and save result file paths in swa-data.
   creates a nested hashtable structure in swa-data with the following hierarchy:
   client-static bundle-id output-format compiled-source-path"
  (let*
    ( (data-ht
        (or (ht-ref (swa-env-data swa-env) (q client-static))
          (let (a (ht-create-symbol)) (ht-set! (swa-env-data swa-env) (q client-static) a) a)))
      (bundles config))
    (each
      (l (a)
        (let ((bundle (first a)) (format-and-source (tail a)) (format-ht (ht-make-eq)))
          (ht-set! data-ht bundle format-ht)
          (each
            (l (a)
              (let*
                ( (format (first a)) (bindings-and-sources (tail a))
                  (first-is-bindings (list? (first bindings-and-sources)))
                  (bindings (and first-is-bindings (first bindings-and-sources)))
                  (sources (if first-is-bindings (tail bindings-and-sources) bindings-and-sources)))
                (ht-set! format-ht format
                  (client-file swa-env format
                    bindings sources (string-append "_" (symbol->string bundle))))))
            format-and-source)))
      bundles)))

(define (client-static swa-env format bundle-ids)
  "vector symbol symbol symbol -> false/string
   get compiled source paths for bundle-ids"
  (let (bundles-ht (ht-ref (swa-env-data swa-env) (q client-static)))
    (map
      (l (a)
        (let (bundle-ht (ht-ref bundles-ht a))
          (if bundle-ht (ht-ref bundle-ht format)
            (raise (list (q client-static-bundle-not-found) a)))))
      bundle-ids)))
