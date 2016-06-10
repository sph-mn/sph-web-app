(library (sph web app client)
  (export
    client-delete-all-compiled-files
    client-html
    client-lang->env
    client-script
    client-style
    client-templates
    lang->pre-process-one
    lang->source-suffix
    lang->target-name)
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
  (define default-env (apply environment (ql (rnrs base) (sph))))

  (define-as type->suffixes-ht symbol-hashtable
    script (list ".sjs" ".js") style (list ".plcss" ".css") html (list ".sxml" ".html"))

  (define (type->suffixes type) (hashtable-ref type->suffixes-ht type (list "")))

  (define-as suffix->lang alist-q
    ".css" css ".plcss" plcss ".sjs" sescript ".js" javascript ".html" html ".sxml" sxml)

  (define (file-name->lang a)
    (any
      (l (suffix-and-lang) (and (string-suffix? (first suffix-and-lang) a) (tail suffix-and-lang)))
      suffix->lang))

  (define-as suffix->pre-process-one symbol-hashtable
    ;these procedures translate the sxml result that (lang template) produces into the target format (strings usually)
    ".sjs" (l (a port) (sescript-use-strict port) (sescript->ecmascript a port))
    ".plcss" plcss->css sxml sxml->xml)

  (define-as lang->env symbol-hashtable
    ;templates are evaluated with their own environments for inline-code/unquote evaluation
    sescript default-env plcss default-env sxml default-env)

  (define client-script-post-process
    (let (uglifyfs (search-env-path-variable "uglifyjs"))
      (and uglifyfs
        (l (local-path)
          (let (temp-path (string-append local-path ".in")) (rename-file local-path temp-path)
            (if (config-ref development)
              (system
                (string-join (list uglifyfs "--beautify" "--output" local-path temp-path) " "))
              (system
                (string-join
                  (list uglifyfs "--compress"
                    "--mangle" "--screw-ie8" "--output" local-path temp-path)
                  " ")))
            (delete-file temp-path))))))

  (define-as lang->post-process symbol-hashtable
    ;these procedures translate the sxml result that (lang template) produces into the target format (strings usually)
    sescript client-script-post-process)

  (define-syntax-rule (path-add-branch-prefix a)
    ;"string -> string"
    (if (string-prefix? "lib" a) "" "branch/"))

  (define (search-path a)
    "string -> string/false
    search in swa-paths to complete a relative path"
    (any (l (e) (let (path (string-append e a)) (if (file-exists? path) path #f))) swa-paths))

  (define (relative-source->path a type)
    (let
      (create-path
        (pair? a
          (l (suffix)
            (string-append (path-add-branch-prefix (first a)) (first a) "/client/" (tail a) suffix))
          (l (suffix) (string-append (path-add-branch-prefix a) a suffix))))
      (any
        (l (suffix)
          (let* ((path (create-path suffix)) (path-found (search-path path)))
            (identity-if path-found
              (begin (log-message (q error) (string-append "missing file \"" path "\"")) #f))))
        (type->suffixes type))))

  (define (directory-fold path proc init)
    (let (d (if (string? path) (opendir path) path))
      (let loop ((e (readdir d)) (result init))
        (if (eof-object? e) result (loop (readdir d) (proc e result))))))

  (define (client-delete-all-compiled-files)
    "deletes all previously generated client-code files to remove old files that would not be generated again"
    (each
      (l (e)
        (let (path (string-append swa-root "root/" e "/"))
          (if (file-exists? path)
            (directory-fold path
              (l (e result) (if (string-prefix? "_" e) (delete-file (string-append path e)))) #f))))
      (vector->list (hashtable-values lang->type))))

  (define (client-template-target source type)
    "any:template-source string -> string:path
    creates the full target path for one compiled template file"
    (string-append swa-root "root/" type "/_" (number->string (equal-hash source) 32)))

  (define (client-template-source source type)
    "template-source string -> template-source
    convert relative paths in template-source to full-paths.
    source can be a pair of the form (path-prefix . path-suffix),
    called context, which is used to create a path with an automatically determined infix"
    (every-map
      (l (merged)
        (if (string? merged)
          (if (string-prefix? "/" merged) merged (relative-source->path merged type))
          (if (pair? merged)
            (if (list? merged)
              (map
                (l (wrapped)
                  (if (string? wrapped) (relative-source->path wrapped type)
                    (if (pair? wrapped)
                      (if (list? wrapped) wrapped (relative-source->path wrapped type)) wrapped)))
                merged)
              (relative-source->path merged type))
            merged)))
      (any->list-s source)))

  (define-syntax-rule (swa-full-path->app-path a) (string-drop a (+ (string-length swa-root) 4)))

  (define (client-templates target type bindings . source)
    "boolean/port:target symbol alist list ... -> datum/list:paths/unspecified
    the template-handler used by client-html/script/style.
    uses the (sph lang template) source format and extends it with"
    (let (auto-target (and (boolean? target) target))
      (if target
        (if auto-target
          (filter-map
            (l (e)
              (let (target (client-template-target e type))
                (if (and (file-exists? target) (not (config-ref development)))
                  (swa-full-path->app-path target)
                  (let
                    ( (pre-process-one (hashtable-ref lang->pre-process-one lang))
                      (post-process (hashtable-ref lang->post-process lang)))
                    (if-pass (client-template-source e type)
                      (l (source) (ensure-directory-structure (dirname target))
                        (call-with-output-file target
                          (l (port)
                            (template-fold (l (e result) (pre-process-one e port) result) source
                              bindings env target)))
                        (if post-process (post-process target)) (swa-full-path->app-path target)))))))
            source)
          (each
            (let (pre-process-one (hashtable-ref lang->pre-process-one lang))
              (l (e)
                (if-pass (client-template-source e type)
                  (l (source)
                    (template-fold (l (e result) (pre-process-one e target)) source bindings env #f)))))
            source))
        (apply append
          (map
            (l (e)
              (if-pass (client-template-source e type)
                (template-fold pair source bindings env (list))))
            source)))))

  (define (client-html target bindings . sources)
    "port alist-q template-source ... ->
    displays an html <!doctype html> doctype and writes the results of evaluating template-sources to target"
    (display "<!doctype html>" target) (apply client-templates target (q html) bindings sources))

  (define (client-script bindings . sources)
    "alist-q template-source ... -> string
    evaluates sources as sescript"
    (apply client-templates #t (q script) bindings sources))

  (define (client-style bindings . sources)
    "alist-q template-source ... -> string
    evaluates sources as plcss"
    (apply client-templates #t (q style) bindings sources)))
