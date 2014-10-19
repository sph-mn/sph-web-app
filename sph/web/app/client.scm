;client-code processing

(library (sph web app client)
  (export
    client-html
    client-init
    client-lang->env
    client-script
    client-style
    client-templates
    lang->source-suffix
    lang->target-name
    lang->translate)
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

  (define default-env (apply environment (ql (rnrs base) (sph))))

  (define-as lang->source-suffix symbol-hashtable
    ;the file-name suffixes to look for when requesting template files in a specific language
    sescript ".sjs" plcss ".plcss" sxml ".sxml" html ".html")

  (define-as lang->target-name symbol-hashtable
    ;the root/-relative target directories to put compiled templates into
    sxml "html" sescript "script" plcss "style")

  (define-as client-lang->env symbol-hashtable
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

  (define-as lang->translate symbol-hashtable
    ;these procedures translate the sxml result that (lang template) produces into the target format (strings usually)
    sescript (l (a port) (sescript-use-strict port) (sescript->ecmascript a port))
    plcss plcss->css sxml sxml->xml)

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

  (define-syntax-rule (source->path a suffix)
    ;string:path string:suffix -> string
    (let (path (string-append (path-add-branch-prefix a) a suffix))
      (or (search-path path)
        (begin (log-message (q error) (string-append "missing file \"" path "\"")) #f))))

  (define-syntax-rule (context-source->path a suffix)
    ;(path-prefix . path-suffix) string:suffix -> string
    (let*
      ( (context (first a))
        (path (string-append (path-add-branch-prefix context) context "/client/" (tail a) suffix)))
      (or (search-path path)
        (begin (log-message (q error) (string-append "missing file \"" path "\"")) #f))))

  (define (directory-fold path proc init)
    (let (d (if (string? path) (opendir path) path))
      (let loop ((e (readdir d)) (result init))
        (if (eof-object? e) result (loop (readdir d) (proc e result))))))

  (define (client-init)
    "deletes all previously generated client-code files to remove old files that would not be generated again"
    (each
      (l (e)
        (let (path (string-append swa-root "root/" e "/"))
          (if (file-exists? path)
            (directory-fold path
              (l (e result) (if (string-prefix? "_" e) (delete-file (string-append path e)))) #f))))
      (vector->list (hashtable-values lang->target-name))))

  (define (client-template-target source target-name)
    "any:template-source string -> string:path
    creates the full target path for one compiled template file"
    (string-append swa-root "root/" target-name "/_" (number->string (equal-hash source) 32)))

  (define (client-template-source source suffix)
    "template-source string -> template-source
    convert relative paths in template-source to full-paths"
    (every-map
      (l (merged)
        (if (string? merged) (if (string-prefix? "/" merged) merged (source->path merged suffix))
          (if (pair? merged)
            (if (list? merged)
              (map
                (l (wrapped)
                  (if (string? wrapped) (source->path wrapped suffix)
                    (if (pair? wrapped)
                      (if (list? wrapped) wrapped (context-source->path wrapped suffix)) wrapped)))
                merged)
              (context-source->path merged suffix))
            merged)))
      (any->list-s source)))

  (define-syntax-rule (swa-full-path->app-path a) (string-drop a (+ (string-length swa-root) 4)))

  (define (client-templates target lang bindings . source)
    "boolean/port:target symbol alist list ... -> datum/list:paths/unspecified
    the template-handler used by client-html/script/style"
    (let
      ( (env (hashtable-ref client-lang->env lang))
        (target-name (hashtable-ref lang->target-name lang))
        (source-suffix (hashtable-ref lang->source-suffix lang))
        (auto-target (and (boolean? target) target)))
      (if target
        (if auto-target
          (filter-map
            (l (e)
              (let (target (client-template-target e target-name))
                (if (and (file-exists? target) (not (config-ref development)))
                  (swa-full-path->app-path target)
                  (let
                    ( (translate (hashtable-ref lang->translate lang))
                      (post-process (hashtable-ref lang->post-process lang)))
                    (ensure-directory-structure (dirname target))
                    (pass-if (client-template-source e source-suffix)
                      (l (source)
                        (call-with-output-file target
                          (l (port)
                            (template-fold (l (e result) (translate e port) result) source
                              bindings env target)))
                        (if post-process (post-process target)) (swa-full-path->app-path target)))))))
            source)
          (each
            (let (translate (hashtable-ref lang->translate lang))
              (l (e)
                (pass-if (client-template-source e source-suffix)
                  (l (source)
                    (template-fold (l (e result) (translate e target)) source bindings env #f)))))
            source))
        (apply append
          (map
            (l (e)
              (pass-if (client-template-source e source-suffix)
                (template-fold pair source bindings env (list))))
            source)))))

  (define (client-html target bindings . sources)
    "port symbol-alist template-source ... ->
    displays an html <!doctype html> doctype and writes the results of evaluating template-sources to target"
    (display "<!doctype html>" target) (apply client-templates target (q sxml) bindings sources))

  (define (client-script bindings . sources)
    "symbol-alist template-source ... -> string
    evaluates sources as sescript"
    (apply client-templates #t (q sescript) bindings sources))

  (define (client-style bindings . sources)
    "symbol-alist template-source ... -> string
    evaluates sources as plcss"
    (apply client-templates #t (q plcss) bindings sources)))