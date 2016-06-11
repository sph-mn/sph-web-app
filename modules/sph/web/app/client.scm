(library (sph web app client)
  (export
    client-ac-config)
  (import
    (guile)
    (ice-9 ftw)
    (rnrs base)
    (rnrs eval)
    (sph)
    (sph alist)
    (sph conditional)
    (sph config)
    (sph filesystem)
    (sph hashtable)
    (sph lang plcss)
    (sph lang sescript)
    (sph lang template)
    (sph list)
    (sph log)
    (sph tree)
    (sph web app config)
    (sxml simple)
    (only (rnrs hashtables) equal-hash)
    (only (sph process) process-create-chain-with-pipes)
    (only (sph two) search-env-path-variable)
    (only (srfi srfi-1) filter-map))

  ;client-code processing
  (define (has-suffix-proc suffix) (l (a) (string-suffix? suffix a)))
  (define (output-sources-copy a) (thunk (each (l (a) (a (current-output-port))) a)))
  (define default-env (apply environment (ql (rnrs base) (sph))))
  (define uglifyjs-installed? (search-env-path-variable "uglifyjs"))
  (define cleancss-installed? (search-env-path-variable "cleancss"))
  (define cssbeautify-installed? (search-env-path-variable "cssbeautify-cli"))
  (define html-installed? (search-env-path-variable "html"))

  (define javascript-output-compress
    (if uglifyjs-installed?
      (l (config sources port)
        (process-create-chain-with-pipes sources port
          output-sources-copy (list "uglifyjs" "--compress" "--mangle" "--screw-ie8")))
      ac-output-copy))

  (define javascript-output-format
    (if uglifyjs-installed?
      (l (config sources port)
        (process-create-chain-with-pipes sources port
          output-sources-copy (list "uglifyjs" "--beautify")))
      ac-output-copy))

  (define css-output-compress
    (if cleanss-installed?
      (l (config sources port)
        (process-create-chain-with-pipes sources port output-sources-copy (list "clean-css")))
      ac-output-copy))

  (define css-output-format
    (if cssbeautify-installed?
      (l (config sources port)
        (process-create-chain-with-pipes sources port
          output-sources-copy (list "cssbeautify-cli" "--stdin" "--indent=2")))
      ac-output-copy))

  (define html-output-format
    (if html-installed?
      (l (config sources port)
        (process-create-chain-with-pipes sources port output-sources-copy (list "html")))
      ac-output-copy))

  (define (s-template-sxml->xml config sources port)
    (template-fold (l (template-result . result) (sxml->xml template-result port) result)
      (and config (hashtable-ref config (q template-bindings)))
      (or (and config (hashtable-ref config (q template-environment))) default-env) sources))

  (define (s-template-sescript->javascript config sources port)
    "sescript compilation with s-template support"
    (let
      (sescript-load-paths
        (or (and config (hashtable-ref config (q sescript-load-paths))) ses-default-load-paths))
      (template-fold
        (l (template-result . result)
          (sescript->ecmascript template-result port sescript-load-paths) result)
        (and config (hashtable-ref config (q template-bindings)))
        (or (and config (hashtable-ref config (q template-environment))) default-env) sources)))

  (define (s-template-plcss->css config sources port)
    (template-fold (l (template-result . result) (plcss->css template-result port) result)
      (and config (hashtable-ref config (q template-bindings)))
      (or (and config (hashtable-ref config (q template-environment))) default-env) sources))

  ;todo: test sxml compile / sxml composition / css compile, re-create client-*, (display "<!doctype html>" target)

  (define-as client-ac-config symbol-hashtable
    javascript
    (list
      (symbol-hashtable production javascript-output-compress development javascript-output-format)
      (record ac-lang-input (q sescript) (has-suffix-proc ".sjs") s-template-sescript->javascript))
    html
    (list (symbol-hashtable)
      (record ac-lang-input "sxml" (has-suffix-proc ".sxml") s-template-sxml->xml))
    css
    (pair (symbol-hashtable production css-output-compress development css-output-format)
      (record ac-lang-input (q plcss) (has-suffix-proc ".plcss") s-template-plcss->css)))

  (define-as client-format->suffixes symbol-hashtable
    javascript (list ".sjs" ".js") css (list ".plcss" ".css") html (list ".sxml" ".html"))

  (define client-target-directory (string-append swa-root "root/"))

  (define (client-delete-all-compiled-files)
    "deletes all previously generated client-code files to remove old files that would not be generated again"
    (each
      (l (format)
        (let (path (string-append client-target-directory format "/"))
          (if (file-exists? path)
            (directory-fold path
              (l (e result) (if (string-prefix? "_" e) (delete-file (string-append path e)))) #f))))
      (map symbol->string (vector->list (hashtable-keys client-ac-config)))))

  (define (path-relative->path-full path format)
    (identity-if
      (or (swa-search-load-paths path)
        (any (l (suffix) (swa-search-load-paths (string-append path suffix)))
          (client-format->suffixes format)))
      (begin (log-message (q error) (string-append "missing file \"" path "\"")) #f)))

  (define (client-prepare-input-spec output-format input-spec enter-list?)
    (every-map
      (l (merged)
        (cond
          ( (string? merged)
            (if (string-prefix? "/" merged) merged (path-relative->path-full merged output-format)))
          ( (list? merged) (client-prepare-source-spec-one output-format merged #f)
            ((pair? merged) (path-relative->path-full merged output-format))))
        input-spec)))

  ;--
  (define-syntax-rule (swa-full-path->app-path a) (string-drop a (+ (string-length swa-root) 4)))
  (define (client-port port-output target-format bindings . source))
  (define (client-file target-format bindings . source))

  (define (client-html port-output bindings . sources)
    "port alist-q template-source ... ->
    displays an html <!doctype html> doctype and writes the results of evaluating template-sources to target"
    (apply client-port target (q html) bindings sources))

  (define (client-html bindings . sources)
    "port alist-q template-source ... ->
    displays an html <!doctype html> doctype and writes the results of evaluating template-sources to target"
    (apply client-templates target (q html) bindings sources))

  (define (client-script-file bindings . sources)
    "alist-q template-source ... -> string
    evaluates sources as sescript"
    (apply client-templates #t (q script) bindings sources))

  (define (client-style-file bindings . sources)
    "alist-q template-source ... -> string
    evaluates sources as plcss"
    (apply client-templates #t (q style) bindings sources)))
