(define-module (sph web app base))

(use-modules (sph) (sph hashtable)
  (sph lang config) (sph vector) ((sph filesystem) #:select (ensure-trailing-slash)))

(export sph-web-app-start-description swa-app-deinit
  swa-app-init swa-app-new
  swa-app-respond swa-config-get
  swa-env-config swa-env-config-set!
  swa-env-data swa-env-data-set! swa-env-new swa-env-root swa-env? swa-start)

(define sph-web-app-start-description "core web app features")
(define swa-app-respond (vector-accessor 1))
(define swa-app-init (vector-accessor 2))
(define swa-app-deinit (vector-accessor 3))
(define swa-env-root (vector-accessor 1))
(define swa-env-config (vector-accessor 2))
(define swa-env-data (vector-accessor 3))

(define* (swa-app-new respond #:key init deinit)
  "symbol/(symbol ...) procedure:{vector:request -> vector:response} _ ... -> vector:swa-app
   #:init procedure:{vector:swa-env -> swa-env}
   #:deinit procedure:{vector:swa-env -> swa-env}
   create a swa-app object that encapsulates the applications main procedures"
  (vector (q swa-app) respond (or init identity) (or deinit identity)))

(define (swa-env-new root config data) (vector (q swa-env) root config data))
(define (swa-env? a) (and (vector? a) (= 4 (vector-length a)) (eq? (q swa-env) (vector-first a))))

(define (swa-config-get-file swa-root name)
  "string string/hashtable/false -> list
   get a hashtable for the configuration file identified by \"name\".
   if a configuration file with the name default exists, it is loaded first and
   unless name is default, the configuration file identified by name overwrites values
   in the default config.
   for names other than \"default\" a configuration file must exist"
  (let*
    ( (default-path (string-append swa-root "config/default"))
      (config
        (or (and (file-exists? default-path) (config-read-file default-path #:hashtable #t))
          (ht-create-symbol)))
      (name-config
        (and (not (string= "default" name))
          (config-read-file (string-append swa-root "config/" name) #:hashtable #t))))
    (if name-config (ht-tree-merge! config name-config)) config))

(define (swa-config-get swa-root config) "string string -> hashtable"
  (if (string? config) (swa-config-get-file swa-root config) (or config (ht-create-symbol))))

(define (swa-start swa-app root config c . arguments)
  "vector false/string/hashtable procedure:{env app any:arguments ...} any ... -> any
   * find project root from swa-app-name and load-path. module named like swa-app expected to be in project-root/modules/project-name.scm
   * load configuration
   * create the swa-env object and call procedure c, where the app is to be initialised in"
  (let (root (if root (ensure-trailing-slash root) ""))
    (apply c (swa-env-new root (swa-config-get root config) (ht-create-symbol)) swa-app arguments)))
