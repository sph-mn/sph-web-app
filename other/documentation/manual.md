# web-app manual

# project creation
web-app projects are stored in one directory. the command ``web-app``, which should have been installed with sph-web-app, can create a rudimentary project structure. it can also create an example project that showcases features. it is not necessary to use this command but it may be helpful for beginners

```
parameters
  options ... target
description
  creates new (web app) projects
options
  --example  creates an example project
  --help | -h
  --interface
```

execute

```
web-app project-name
```

to create a new project in the current directory. replace project-name with the name of your project

# the main module
* "modules/project-name.scm" should be a scheme module and export at least one binding: ``swa-app`` - a swa-app object (a vector)
* this module is to be the main application entry point for responding to requests as well as what is needed for application initialisation and deinitialisation
* an application can be completely defined in one module. for bigger applications it usually makes sense to split the code into multiple modules
* to create a swa-app object use ``swa-create``
```
(swa-create name respond #:key init deinit depends)
```

example ``modules/project-name.scm``

```
(library (project-name)
  (export swa-app)
  (import
    (rnrs base)
    (sph web app)
    (sph web app http))

  (define (app-respond request)
    (respond "test"))

  (define swa-app (swa-create (quote project-name) app-respond)))
```

``(sph web app http)`` contains the ``respond`` binding for http response objects

# startup
the application and server is best started from a separate executable file, for example "exe/start"

example file ``exe/start``
```
#!/usr/bin/guile
!#
(import (sph web app) (project-name))
(swa-start swa-app "default" swa-server-scgi)
```

``project-name`` needs to be in guiles load path to be found as a module (``$GUILE_LOAD_PATH``, ``guile -L``, ``%load-path``).
when the file has the filesystem executable permission bit set (``chmod +x exe/start``), it can be run from the project root with:

```
./exe/start
```

displayed should be something like:

```
listening on /tmp/1000/scgi
exit with ctrl+c
```

which means the server is running and the application ready for use.

note that scgi is an intermediate interface for communication between a proxy server and the application and that in this case a scgi capable proxy server will be required to access the application via http, see below

signature of swa-start
```
swa-app false/hashtable/string:configuration server server-arguments ... -> unspecified
```

# proxy setup
a proxy is not necessary if instead of swa-server-scgi, swa-server-guile is used. however, scgi is the recommended way because scgi is an interface that allows an established http server to do the work of http request parsing and connection management - and there are servers that do this well, stable and fast.
for example nginx allows for fast file transfers without redirection through the application (x-accel), https, keep-alive requests, chunked encoding, gzip compression, load-balancing and much more. to do this, set up a server to interface with the socket that is created when starting the application with the scgi server (the path is displayed when the server starts). nginx, apache and lighttp support it

see [nginx example configuration for scgi and web-app](nginx.md)

# requests
``app-respond`` from the main module receives a request object

## fields
* path: ``string``, current request path
* query: ``false/((string . any) ...)``, parsed query string, but only if ``#:parse-query? #t`` was passed to swa-start
* headers: ``((string . any) ...)``, alist of parsed http headers
* client: ``input/output port``, can be used for reading from the client. should not be used for writing but a procedure should be used in the response object, so that the server can write the headers before sending the response body
* swa-env: ``vector``
* data: ``false/hashtable``, custom data can be set here

## example field access
```
(swa-http-request-path request)
```

## path based routing
```
(import (sph web app) (sph web app http))

(define (app-respond request)
  (match-path (swa-http-request-path request)
    (() (start-page request))
    (("c" "browse") (c-browse request #f (list)))
    (("c" "browse" type tags ...) (c-browse request type tags))
    (("robots.txt") (respond-type (quote text) "User-Agent: *\nDisallow: /c/browse/link/"))
    (_ (respond 404))))
```

``match-path`` is [(ice-9 match)](https://www.gnu.org/software/guile/manual/html_node/Pattern-Matching.html) except that the path is without the root path, split at slashes, before being passed to match. all features of (ice-9 match) can be used

## swa-env
requests contain a swa-env record, which contains the environment information for the currently initialised application. there is only one such object per application. if it is modified, changes will be available to all requests

### fields
* root: string, the full path to the current top-level project
* paths: hashtable, maps project identifiers to project paths
* config: hashtable, user configuration loaded from the configuration files. not to be modified
* data: hashtable, for custom run-time values, for example values calculated when the application was initialised

### example field access
```
(swa-env-data swa-env)
```

# response creation
the expected response object type depends on the ``send-response`` procedure used by the current server. by default this is ``swa-http-send-response`` from [(sph web app http)](http://sph.mn/c/view/dm). different servers or response sender procedures can be used for protocols other than http

after importing ``(sph web app http)`` you can use the following short respond bindings to create http responses. use them to create procedure results subsequently returned from app-respond

## respond
examples
```
(respond 200)
(respond "test")
(respond 200 "test")
(respond 200 (list) "test")
(respond (lambda (client) (display "test" client)))
(respond 200 (list "content-type:text/plain\r\n") (lambda (client) (display "test" client)))
```

### signature
* form 1
  * ``false/integer/string/vector/procedure -> vector``
  * ``status-code/response-body/swa-http-response/{port:client -> unspecified} -> swa-http-response``
* form 2
  * ``integer false/string/vector/procedure -> vector``
* form 3
  * ``integer list:(string:crlf-terminated-header-line ...) false/string/vector/procedure -> vector``

### description
creates an http response record. the argument for the response-body parameter is interpreted by the sender procedure as follows:

* procedure: called with a port for bidirectional communication with the client
* integer: http status code, empty response-body
* string: http 200, string as response-body
* boolean-false: 404
* vector: a swa-http-response-record
* other: http 200, empty response-body

## respond-type
examples
```
(respond-type (quote json) "{\"test\": 1}")
(respond-type (quote text) 200 (list) "test")
```
### signature
```
symbol:content-type-identifier respond-argument ... -> swa-http-response
```
### description
* like ``respond`` but takes an additional argument for the response content-type
* the content type identifiers supported by default are: ``json``, ``html``, ``js``, ``css``, ``text``
* the hash-table ``swa-http-key->mime-type`` defines the content types and can be extended

## respond-html
examples
```
(respond-html #f "testfile")
(respond-html #f (list "append-this-file" "and-this-file" "and-this-one"))
(respond-html #f (list "append-this-file-with" (list "into-this-file" "insert-this-file" "insert-insert-this-file") "append-this"))
(respond-html #f (list (list (list (quote div) "sxml starts here"))))
(respond-html #f (quote (((div "sxml starts here")))))
(respond-html #f (list (list (lambda (v content) "template procedure") "testfile-for-content")))
(respond-html (quote ((myvariable . 2) (othervariable . 3))) (list (lambda (v content) (v (quote myvariable)))))
```

### signature
```
false/((symbol:key . any:value) ...):alist:variables template-source ... -> swa-http-response
```

### description
this uses client-html from [(sph web app client)](http://sph.mn/c/view/7u).
sources are given in "template-source" format which is simple to use but supports several ways to specify sources and has the following type-signature

```
string:path/procedure/port -> swa-http-response
```
or
```
(string:path/procedure/port/(string:path:wrapped/procedure/port/list:sxml ...) ...) -> swa-http-response
```

elements on the first level of the list are appended, elements on the second nesting level are composed (inserted from right to left into each other using the content variable available in templates), elements on the third nesting level are template content specified directly

template procedures have the signature
```
procedure:{symbol:variable-name [any:default] -> any}:v any:content -> sxml
```

the first argument to template procedures is a procedure that returns the values of template variables.
the second argument is the content received from the previous template value for composition, or false if there is none

# configuration files
## where
configuration files are stored under ``config/`` with the filename suffix ``.scm``

## syntax
* the content of configuration files is interpreted like elements of a quasiquoted list with key and value specified alternatingly. full scheme syntax is supported. indendation is not relevant (= scheme syntax)
* lists and sub-lists create key-value hierarchies. when lists are prefixed with "..", actual lists are created in the config object instead of nested hashtables
* web-app itself only has few configuration options, all other keys and values are user controlled and custom. use them in the configuration file and they will be available in the application

example ``config/default.scm``
```
default-title "project-name"
server (
  socket-name "project-name"
  socket-permissions #o770
  socket-group "http")
types (.. 2 4 5)
```

## derivation
other configuration files derive from "default.scm". in other configuration files, any key from default that is not used takes its default value from default.scm.
for example, if there is a file "config/development.scm" that contains only

```
mode development
```

and swa-start is instructed to use this configuration with the name development, then the configuration will contain all values from default.scm, overwritten with all values from development.scm.
this can be used to create multiple configuration files with minor differences for different environments, for example for different server hosts

## access
configuration will be available as a hashtable in swa-env in the field "config". it can be retrieved by calling (swa-env-config swa-env)

example
```
(let (config (swa-env-config swa-env))
  (hashtable-ref config (quote option-name)))
```

``(sph hashtables)`` contains helpers to work with nested hashtables, example

```
(ht-tree-ref-q config option-name nested-option-name nested-nested-option-name)
```

``-q`` indicates that the values used for keys are automatically quoted

# directory structure
there is no fundamentally required directory structure. there are some required directories depending on optional features used. if you specify a configuration name, "config/" is needed, if you use (sph web app client), "client/" and "root/" is needed, but the rest can be freely adapted

nevertheless, there is a recommended directory structure for web-app. in the following listing, words with filename extensions are files and words enclosed with {} are non-literal, user-chosen names, other words are directories and indent marks directory structure nesting

```
{project-name}
  modules
    {project-name}.scm
    {project-name}
      controller
      helper
      http
      model
      other
      view
    test
      module helper
  client
    {output-format-extension} ...
      {name}.{input-format-extension}
  config
    default.scm
    {config-name}.scm
  exe
    start
  root
    assets
      {output-format-extension} ...
  other temp data
```

* root: the only directory directly accessible for web server clients. the document root for a (proxy) web server. contains compiled client code files under assets and any static files that should be accessible to clients without redirection through the application. the name "root" was chosen in favor of "public" or "shared"
* exe: contains executable files for general project management. for example for starting the application or custom helper scripts. the name "exe" was chosen in favor of "bin" or "script"
* client: the client directory contains code or sources and templates for code that is to be evaluated by the client. for example html, css or javascript
* http: procedures that take http requests and create http responses
* controller: combines model and view to create the response content. prefix bindings with c- (controller, model and view tend to use the same names for bindings)
* model: returns data for controllers, for example from a database. prefix bindings with m-
* view: returns what is necessary for presenting the data from the model. for example directly generated shtml not read from files under client/. prefix bindings with v-
* other: modules that do not fit into the other directories. lib
* test/module: contains scheme modules for running tests. for example using (sph test)
* test/helper: contains helper modules for writing tests
* data: for application content data like database files

examples
* [smaller project that does not use a database](http://files.sph.mn/sourcecode/ytilitu)
* [a content management system](http://files.sph.mn/sourcecode/sph-cms)

# serving static files
## with scgi and an http proxy server
configure the server to serve files from the ``root/`` directory in the web-app project directly, without forwarding incoming file requests to the scgi application

## without scgi and an http proxy server
* the file sending is the responsibility of the application
* match file requests as appropriate, read files and write their content to the client port

# client code
* source formats recognised by default: html, css, javascript, sxml, plcss, sescript
* target formats supported by default: html, css, javascript
* code to be executed by the client can be created directly where needed in scheme procedures or loaded from external files under ``client/{format-name}/``
* import (sph web app client) to use the following features. it supports most common ways of file pre-processing, including pre-compilation, concatenation, compression, formatting, templating, template variables and custom processors for any file format
* see also the library documentation of [(sph web app client)](http://sph.mn/c/view/7u)

## template files
default processors accept source specifications in the [(sph lang template)](http://sph.mn/c/view/q6) format. s-expression based formats can use template variables via unquote and the v procedure in the code

```
(div (@ (class test)) (unquote (v (quote example-variable))))
```

```
(div
  (unquote
    (if (v (quote example-variable))
      (qq (span "a"))
      (qq (span (@ (class test-class)) "b")))))
```

## static files
create a ``client-static`` config object and ``client-static-config-create`` defines file bundles that have ids

```
(import (sph web app client))

(define client-static-config
  (client-static-config-create project-name
    (default
      js (#f "main")
      css ((example-variable "testvalue" another-variable 3) "main" "otherfile"))
    (otherbundle js (#f "otherfile"))))
```

sources are specified as for ``client-file`` and relative paths are read from ``{swa-root}/client/{output-format}/{relative-path}``. all available supported formats can be used, for example also plain html and html from sxml. and the source files can also be sjs or plcss, with the file path ``client/css/main.plccs`` for example

in app-init, call
```
(client-static-compile swa-env client-static-config)
```

where needed, get the public, server-root relative, path to one or multiple compiled bundle files with client-static
```
(client-static swa-env (quote project-name) (quote css) (quote (bundle-name other-bundle-name)))
```

## dynamic files
``(sph web app client)`` has exports to pre-process files/templates and sets of those on demand

### client-file
client-file, client-file-html, client-file-css and client-file-js write the pre-processed result to a file with an automatically generated file name with an underscore prefix (unless set otherwise using the file-name parameter)
```
(client-file swa-env format-name template-variables default-project-id sources #:optional file-name)
(client-file swa-env (quote js) (quote ((a . 2) (b . 3))) (quote myproject) (list "relative-path"))
```

### client
client-html, client-css and client-js write the processed content to port directly
```
(client-html swa-env port bindings project . sources)
```

### other projects
to reference files from projects of which the current root project derives from (see swa-create ``#:depends``), pairs can be used for file names

example
```
(client-file-js swa-env #f (quote myproject) (quote ((otherproject . "relative-path"))))
```

# servers
## included

the following servers are available by default and can be used with swa-start

### sph server scgi
starts a "simple common gateway interface" [(scgi)](http://python.ca/scgi/protocol.txt) server to be used as a backend for an http proxy that supports scgi. if you have heard of fastcgi, scgi is a bit like fastcgi but much simpler

``swa-server-scgi``

### sph server fibers
a scgi server that uses non-blocking input/output with [fibers](https://github.com/wingo/fibers/). usage
```
(import (sph web app) (sph server fibers))
(swa-start swa-app "default" swa-server-scgi
  #:server-listen server-listen-fibers
  #:server-socket server-socket-fibers)
```

caveats of using guile fibers apply (handler should not use any blocking features, need to use rnrs port writers and readers, multiple requests might be processed in the same kernel thread which makes it incompatible with [sph-db](https://github.com/sph-mn/sph-db))

### guile http server
the http server that comes with guile

``swa-server-guile``

### internal
does not create a socket and calls a procedure with the initialised application. can be used for testing. usage
```
(swa-start swa-app "default" swa-server-internal
  (lambda (swa-env app-respond)
    (app-respond (record swa-http-request path arguments headers client swa-env))))
```

## common configuration options
```
server (
  listen-port 1234
  listen-address "::1"
  socket-name "testname"
  socket-group "http"
  socket-permissions #o777)
```

without those options specified the default is to create a local unix socket at the following path

```
/tmp/$UID/scgi
```

### how to set the local socket path
set the ``listen-address`` option in the configuration file to the desired filesystem path

### how to use a tcp socket
set ``listen-address`` to a string with the ip4 or ip6 address to bind to and listen-port to the port number to use

```
server (
  listen-address "::1"
  listen-port 6500)
```

# deriving from projects
use the ``#:depends`` parameter of swa-create.
all ``modules/`` directories of projects should be in ``GUILE_LOAD_PATH`` or similar, then import application modules as scheme libraries where needed

for example in modules/sph-mn.scm
```
(import (sph web app) (prefix (sph-cms) cms-))

(define (app-init swa-env)
  ; calls the init routine of the other project to use its special features
  ((swa-app-init cms-swa-app) swa-env))

(define (app-respond request)
  (respond "i'm derived"))

(define swa-app
  (swa-create (quote sph-mn) app-respond
    #:init app-init #:deinit (swa-app-deinit cms-swa-app) #:depends (quote sph-cms)))
```

this imports the swa-app object from (sph-cms) with the identifier prefixed with cms- to avoid a name conflict in the current module, then uses the swa-app object accessor swa-app-init to retrieve the other app-init procedure and calls it with the swa-env.
static files from imported project root/ directories will be symlinked into the parent projects root/ unless their file name starts with an underscore. static files that are compiled will be saved in the parent project

# deployment
the target host needs to have guile and sph-web-app set up. setup is the same as for local development environments

example systemd service file ``/etc/systemd/system/sph-mn.service``
```
[Unit]
Description=sph-mn scgi application

[Service]
Environment="GUILE_LOAD_PATH=/opt/web-app/sph-cms/modules:/opt/web-app/sph-mn/modules:/usr/share/guile/2.2:/usr/share/guile/site"
WorkingDirectory=/opt/web-app/sph-mn
ExecStart=/opt/web-app/sph-mn/exe/start
Type=simple
Restart=on-failure
User=sph-mn

[Install]
WantedBy=multi-user.target
```

# extending the asset pipeline
see [sph web app client](http://sph.mn/c/view/7u) and [sph filesystem asset-compiler](http://sph.mn/c/view/6q).
extend the hash-table ``client-ac-config``
```
(import (sph filesystem asset-compiler) (sph web app client) (rnrs hashtables))

(hashtable-set! client-ac-config (quote tar)
  (list
    (ac-config-output tar #t ac-output-copy)
    (ac-config-output tar.gz #t
      (lambda (source next-port options)
        (call-with-input-file source (lambda (file) (decompress file next-port)))))))
```

# logging
possible with [(sph log)](http://sph.mn/c/view/7q)
