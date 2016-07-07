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
    client-port)
  (import
    (rnrs base)
    (rnrs eval)
    (sph common)
    (sph config)
    (sph filesystem asset-compiler)
    (sph lang plcss)
    (sph lang sescript)
    (sph lang template)
    (sph log)
    (sph record)
    (sph web app base)
    (sxml simple)
    (except (rnrs hashtables) hashtable-ref)
    (only (guile) current-output-port)
    (only (sph two) search-env-path-variable))

  ;client-code processing
  (define (has-suffix-proc suffix) (l (a) (if (string? a) (string-suffix? suffix a) #t)))
  (define (output-sources-copy a) (thunk (each (l (a) (a (current-output-port))) a)))
  (define-syntax-rule (client-output-directory) (string-append swa-root "root/"))
  (define-syntax-rule (client-output-path) "assets/")
  (define default-env (apply environment (ql (rnrs base) (sph))))
  (define path-uglifyjs (search-env-path-variable "uglifyjs"))
  (define path-cleancss (search-env-path-variable "cleancss"))
  (define path-cssbeautify (search-env-path-variable "cssbeautify-cli"))
  (define path-html (search-env-path-variable "html"))

  (define javascript-output-compress
    (if path-uglifyjs
      (l (config sources port)
        (process-create-chain-with-pipes sources port
          (output-sources-copy sources) (list path-uglifyjs "--compress" "--mangle" "--screw-ie8")))
      ac-output-copy))

  (define javascript-output-format
    (if path-uglifyjs
      (l (config sources port)
        (process-create-chain-with-pipes sources port
          (output-sources-copy sources) (list path-uglifyjs "--beautify")))
      ac-output-copy))

  (define css-output-compress
    (if path-cleancss
      (l (config sources port)
        (process-create-chain-with-pipes sources port
          (output-sources-copy sources) (list path-cleancss)))
      ac-output-copy))

  (define css-output-format
    (if path-cssbeautify
      (l (config sources port)
        (process-create-chain-with-pipes sources port
          (output-sources-copy sources) (list path-cssbeautify "--stdin" "--indent=2")))
      ac-output-copy))

  (define html-output-format
    (if path-html
      (l (config sources port)
        (process-create-chain-with-pipes sources port
          (output-sources-copy sources) (list path-html)))
      ac-output-copy))

  (define (s-template-sxml->html config sources port) (display "<!doctype html>" port)
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

  (define-as client-ac-config symbol-hashtable
    javascript
    (list
      (symbol-hashtable production javascript-output-compress development javascript-output-format)
      (record ac-lang-input (q sescript) (has-suffix-proc ".sjs") s-template-sescript->javascript))
    html
    (list (symbol-hashtable)
      (record ac-lang-input "sxml" (has-suffix-proc ".sxml") s-template-sxml->html))
    css
    (list (symbol-hashtable production css-output-compress development css-output-format)
      (record ac-lang-input (q plcss) (has-suffix-proc ".plcss") s-template-plcss->css)))

  (define-as client-format->suffixes-ht symbol-hashtable
    javascript (list ".sjs" ".js") css (list ".plcss" ".css") html (list ".sxml" ".html"))

  (define-syntax-rule (client-format->suffixes format)
    (hashtable-ref client-format->suffixes-ht format (list)))

  (define (client-delete-compiled-files)
    "deletes all previously generated client-code files to remove old files that would not be generated again"
    (each
      (l (format)
        (let (path (string-append (client-output-directory) (client-output-path) format "/"))
          (if (file-exists? path)
            (directory-fold path
              (l (e result) (if (string-prefix? "_" e) (delete-file (string-append path e)))) #f))))
      (map symbol->string (vector->list (hashtable-keys client-ac-config)))))

  (define (path-add-prefix a format) (string-append "client/" (symbol->string format) "/" a))

  (define (path-relative->path-full path format)
    (let (path (path-add-prefix path format))
      (identity-if
        (or (swa-search-load-paths path)
          (any (l (suffix) (swa-search-load-paths (string-append path suffix)))
            (client-format->suffixes format)))
        (begin (log-message (q error) (string-append "missing file \"" path "\"")) #f))))

  (define (path-pair->path-full a format)
    "(string:load-path-suffix . string:path) symbol -> false/string"
    (let*
      ( (load-path-suffix (ensure-trailing-slash (first a)))
        (path-full
          (any
            (l (e)
              (and (string-suffix? load-path-suffix e)
                (string-append e (path-add-prefix (tail a) format))))
            swa-paths)))
      (if (and path-full (file-exists? path-full)) path-full
        (begin (log-message (q error) (string-append "missing file \"" path-full "\"")) #f))))

  (define (client-prepare-input-spec input-spec output-format enter-list?)
    (and (not (null? input-spec))
      (every-map
        (l (a)
          (cond
            ((string? a) (if (string-prefix? "/" a) a (path-relative->path-full a output-format)))
            ((list? a) (if enter-list? (client-prepare-input-spec a output-format #f) a))
            ((pair? a) (path-pair->path-full a output-format)) (else a)))
        input-spec)))

  (define (swa-mode-get) (if (config-ref development) (q development) (q production)))

  (define (client-port port-output output-format bindings input-spec)
    (let (input-spec (client-prepare-input-spec input-spec output-format #t))
      (and input-spec
        (ac-compile client-ac-config (swa-mode-get)
          port-output output-format input-spec (symbol-hashtable template-bindings bindings)))))

  (define (client-file output-format bindings input-spec)
    (let
      ( (output-directory (client-output-directory)) (mode (swa-mode-get))
        (input-spec (client-prepare-input-spec input-spec output-format #t)))
      (and input-spec
        (if-pass
          (ac-compile->file client-ac-config mode
            (string-append output-directory (client-output-path)) output-format
            input-spec #:only-if-newer
            (equal? mode (q production)) #:processor-config
            (symbol-hashtable template-bindings bindings))
          (l (a) (string-append "/" (string-drop-prefix output-directory a)))))))

  (define (client-javascript port bindings . sources)
    (client-port port (q javascript) bindings sources))

  (define (client-css port bindings . sources) (client-port port (q css) bindings sources))
  (define (client-html port bindings . sources) (client-port port (q html) bindings sources))
  (define (client-javascript-file bindings . sources) (client-file (q javascript) bindings sources))
  (define (client-css-file bindings . sources) (client-file (q css) bindings sources))
  (define (client-html-file bindings . sources) (client-file (q html) bindings sources)))
