(define-module (sph filesystem asset-compiler))

(use-modules (srfi srfi-1) (srfi srfi-2)
  (sph) (sph filesystem) (sph hashtable) (sph io) (sph list) (sph vector))

(export ac-compile ac-compile->file
  ac-config-input ac-config-new
  ac-config-output ac-config-valid?
  ac-file-name ac-input-copy
  ac-input-id ac-input-matcher
  ac-input-processor ac-output-copy
  ac-output-id ac-output-processor
  ac-output-suffix ac-source-files-updated? sph-filesystem-asset-compiler-description)

(define sph-filesystem-asset-compiler-description
  "preprocess files.
   # features
   * configuration object for source file matchers and processors
   * optionally ignore files if already existing and unchanged
   * by default automatically create destination file names from source file names
   # notes
   * ac-input converts data into the output format and ac-output finalises it and writes it to the target file
   * ac-config is a hashtable:{symbol:output-format -> (ac-output ac-input ...)}
   # example
   (define client-ac-config
     (ac-config-new
       (list
         (list (q html)
           (ac-config-output (q html) \".html\" ac-output-copy)
           (ac-config-input (q html) \".html\" ac-input-copy)
           (ac-config-input (q shtml) \".shtml\" template-sxml->html)
           (ac-config-input (q shtml-data) (const #t) template-sxml->html))
         (list (q css)
           (ac-config-output (q css) \".css\" css-output-processor)
           (ac-config-input (q css) \".css\" ac-input-copy)
           (ac-config-input (q plcss) \".plcss\" template-plcss->css)
           (ac-config-input (q plcss-data) (const #t) template-plcss->css)))))
   (define (input-processor source port options)
     (sxml->xml template-result port)
     (newline port))")

(define (ac-config-output id suffix processor) "symbol string procedure -> vector"
  (vector (q ac-output) id suffix processor))

(define (ac-config-input id matcher processor) "symbol procedure/string procedure -> vector"
  (vector (q ac-input) id
    (if (procedure? matcher) matcher
      (if (string? matcher) (l (a) (and (string? a) (string-suffix? matcher a))) (const #f)))
    processor))

(define ac-input-id (vector-accessor 1))
(define ac-input-matcher (vector-accessor 2))
(define ac-input-processor (vector-accessor 3))
(define ac-output-id (vector-accessor 1))
(define ac-output-suffix (vector-accessor 2))
(define ac-output-processor (vector-accessor 3))

(define (ac-config-new config-source) "list:((id config-output config-input ...) ...) -> hashtable"
  (ht-from-alist config-source))

(define (ac-input-copy source port options)
  "a default processor-input that interprets source as a file name if it is a string.
   copies all contents of the source file to port with a newline at the end.
   if source is not a string it does nothing and returns false"
  (and (string? source) (begin (file->port source port) (newline port))))

(define (ac-output-copy write-input port options)
  "a default processor-output that copies input to output port" (write-input port))

(define (ac-sources->input-processor sources config-input options)
  "list list any -> procedure
   map each source to one processor and return a procedure that calls all processors with the
   associated source and writes to a port"
  (let
    (processor-and-source
      (map
        (l (a)
          (or
            (any (l (b) (and ((ac-input-matcher b) a) (pair (ac-input-processor b) a)))
              config-input)
            (raise (list (q no-matching-input-processor) a))))
        sources))
    (l (out-port) "call each input processor with associated source, output port and options"
      (each (l (a) ((first a) (tail a) out-port options)) processor-and-source))))

(define* (ac-compile config output-format sources port #:optional processor-options)
  "hashtable symbol list port [any] -> false/unspecified
   \"processor-options\" is an optional custom value passed to input and output processors as the last argument"
  (and-let* ((config-format (ht-ref config output-format)))
    ( (ac-output-processor (first config-format))
      (ac-sources->input-processor sources (tail config-format) processor-options) port
      processor-options)))

(define* (ac-file-name path-directory format sources #:optional name (suffix ""))
  "string symbol string list -> string
   create a string for an output path relative to \"path-directory\".
   format:
   * \"{path-directory}/{format}/{name}\"
   * \"{path-directory}/{format}/_{basename-without-suffix-first-of-sources}-{base32-hash-of-sources}\""
  (string-append (ensure-trailing-slash path-directory) (symbol->string format)
    "/"
    (if name (string-append name suffix)
      (let (strings (filter string? sources))
        (string-append "_"
          (if (null? strings) "_" (first (string-split (basename (first strings)) #\.))) "-"
          (number->string (ht-hash-equal sources) 32) suffix)))))

(define (ac-source-files-updated? dest sources)
  "string (string ...) -> boolean
   true if any source is newer than destination"
  (let (dest-mtime (stat:mtime (stat dest)))
    (any (l (a) (< dest-mtime (stat:mtime (stat a)))) sources)))

(define*
  (ac-compile->file config output-format sources dest-directory #:key processor-options when
    file-name)
  "hashtable symbol list string _ ... -> string:path-destination
   compile a set of sources and get the filesystem path to the compiled file.
   #:processor-options: any/alist
   #:when takes a symbol:
     new: update only if destination does not exist
     newer: update if destination does not exist or any source file is newer
     always: always compile, overwriting any existing destination file
   #:file-name sets the destination file name to use instead of an automatically generated one"
  (let*
    ( (config-format (ht-ref config output-format)) (config-output (first config-format))
      (path-destination
        (ac-file-name dest-directory output-format
          sources file-name (ac-output-suffix config-output))))
    (if
      (or (eq? (q always) when) (not (every string? sources))
        (not (file-exists? path-destination)) (ac-source-files-updated? path-destination sources))
      (and (ensure-directory-structure (dirname path-destination))
        (call-with-output-file path-destination
          (l (port) (ac-compile config output-format sources port processor-options)))
        path-destination)
      path-destination)))
