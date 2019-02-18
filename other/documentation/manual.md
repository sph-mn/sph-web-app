# web-app manual

sph-web-app is a library for processing requests and responses from sockets with helpers for handling http web requests.
the most basic function is to start a server, call a custom handler procedure with request objects and send response objects to the client

## example
content of file `example.scm`
```scheme
(import (sph web app) (sph web app http))

(define (app-respond request) (respond "test"))

(define swa-app (swa-app-new app-respond))

(swa-start swa-app #f #f swa-server-guile)
```

shell command
```shell
guile example.scm
```

shell standard output
```
listening on 127.0.0.1:6500
exit with ctrl+c
```

application browser accessible at
```
http://127.0.0.1:6500
```

# libraries
* `(sph web app)` core features and servers
* `(sph web app http)` http request and response features
* `(sph web app client)` client code or asset processing (css, js, etc)

# app
a swa-app object contains the request handler procedure to be used and optionally procedures called before or after the server runs

## create a new swa-app
```
(swa-app-new app-respond)
```

signatures
```
swa-app-new :: procedure:respond [#:init procedure #:deinit procedure] -> swa-app
respond :: request -> response
init :: swa-env -> any/swa-env
deinit :: swa-env -> any/swa-env
```

example with all options
```
(swa-app-new app-respond #:init app-init #:deinit app-deinit)
```

app-init is called before the server starts, with a swa-env object that contains for example the parsed configuration. app-init can return a new swa-env object to update it.
app-deinit is like app-init but called after the server finished listening

## access swa-app fields
```
swa-app-init :: swa-app -> procedure
swa-app-deinit :: swa-app -> procedure
swa-app-respond :: swa-app -> procedure
```

# startup
to start a server running an app:
```
(swa-start swa-app #f #f swa-server-scgi)
```

signature
```
swa-start :: swa-app directory configuration server server-arguments ... -> unspecified
swa-start :: vector string false/hashtable/string procedure any ... -> unspecified
server :: swa-env swa-app server-arguments ...
```

* directory: a root directory for the application (swa-root), relative to which configuration or client files are searched
* configuration: false for no configuration object, a hashtable for the configuration object or a string for a configuration file name
* server: a server implementation procedure like the included swa-server-scgi, swa-server-guile or swa-server-internal

## swa-env
on startup a `swa-env` object is created. swa-app handlers receive the `swa-env` as an argument or inside the request object.
if a swa-env object is modified, changes are available to all requests.

### fields
* root: string: the full path to the app directory
* config: hashtable: user configuration passed to swa-start or loaded from configuration files
* data: hashtable: for custom run-time values, for example values calculated when the application was initialised

### example field access
```
(swa-env-data swa-env)
```

# configuration files
app configuration can be stored in files that are parsed on startup and available to swa-app handlers. some keys are recognised and used by sph-web-app features, like for example keys for server configuration used by server implementations. otherwise key associations are custom and can be freely made

## where
configuration files are expected to be under `{swa-root}/config/`

## syntax
* all elements are scheme expressions
* key and value are specified alternatingly
* keys are symbols
* indent of two spaces is used for nesting
* all scheme syntax including comments works

example `config/default`
```
default-title "project-name"
server
  socket-name "project-name"
  socket-permissions #o770
  socket-group "http"
types (2 4 5)
```

## derivation
configuration files derive from a configuration file "default" it that exists. in other configuration files, any key from default that is not overwritten keeps its value from default.
for example, if there is a file "config/development" that contains only:

```
mode development
```

and swa-start is instructed to use this configuration with the name development, then the configuration will contain all values from default, overwritten with all values from development.
this can be used to create multiple configuration files with minor differences for different environments, for example for different server hosts

## access
configuration will be available as a hashtable in swa-env in the field "config". it can be retrieved by calling `(swa-env-config swa-env)`

example
```
(let (config (swa-env-config swa-env))
  (hashtable-ref config (quote option-name)))
```

# servers
## included
the following servers are available by default and can be used with swa-start

### sph server scgi
starts a "simple common gateway interface" [(scgi)](http://python.ca/scgi/protocol.txt) server to be used as a backend for an http proxy that supports scgi. if you have heard of fastcgi, scgi is a bit like fastcgi but much simpler

`swa-server-scgi`

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
does not create a socket and calls a procedure with the initialised application. can be used for testing

example
```
(swa-start swa-app (getcwd) "default" swa-server-internal
  (lambda (swa-env app-respond)
    (app-respond (swa-http-request-new path arguments headers client swa-env #f))))
```

## common configuration options
```
server
  listen-port 1234
  listen-address "::1"
  socket-name "testname"
  socket-group "http"
  socket-permissions #o777
```

without those options specified the default is to create a local unix socket at the following path
```
/tmp/$UID/scgi
```

### how to set the local socket path
set the `listen-address` option in the configuration file to the desired filesystem path

### how to use a tcp socket
set `listen-address` to a string with the ip4 or ip6 address to bind to and listen-port to the port number to use

```
server
  listen-address "::1"
  listen-port 6500
```

# http requests
the swa-app respond procedure is called for each request with a request object that contains information about the current request.
the type of the object depends on the server implementation used. for `swa-server-scgi` and `swa-server-guile`, it is a request object from `(sph web app http)`.

## request object fields
* path: `string`, current request path
* query: `false/((string . any) ...)`, parsed query string. only available if `#:parse-query? #t` was passed to `swa-start`
* headers: `((string . any) ...)`, alist of parsed http headers
* client: `input-output-port`, can be used for reading from the client. should not be used for writing as sending the response is done when processing the response object
* swa-env: `vector`
* data: `false/hashtable`, custom data can be set here

## to load swa-http-request related bindings
```
(import (sph web app) (sph web app http))
```

## example field access
```
(swa-http-request-path request)
```

## path based routing
```
(match-path (swa-http-request-path request)
  (() (start-page request))
  (("c" "browse") (c-browse request #f (list)))
  (("c" "browse" type tags ...) (c-browse request type tags))
  (("robots.txt") (respond-type (quote text) "User-Agent: *\nDisallow: /c/browse/link/"))
  (_ (respond 404)))
```

``match-path`` is like [(ice-9 match)](https://www.gnu.org/software/guile/manual/html_node/Pattern-Matching.html), except that the path is without the root path portion split at slashes before being passed to `match`. all features of `(ice-9 match)` can be used

# http responses
for swa-server-scgi and swa-server-guile, the app respond procedure is expected to return swa-app-http-response objects from `(sph web app http).

## create a http response object
examples
```
(respond 200)
(respond "test")
(respond 200 "test")
(respond 200 (list) "test")
(respond (lambda (client) (display "test" client)))
(respond 200 (list "content-type:text/plain\r\n") (lambda (client) (display "test" client)))
```

### signatures
form 1
```
false/integer/string/procedure:{port:client -> unspecified} -> vector
status-code/response-body/response-body-writer -> response
```

form 2
```
integer false/string/procedure -> vector
status-code response-body/response-body-writer -> response
```

form 3
```
integer list:(string:crlf-terminated-header-line ...) false/string/procedure -> vector
status-code headers response-body/response-body-writer -> response
```

### description
* response-body-writer: procedure: called with a port for bidirectional communication with the client after the headers have been sent. for example to read a post request body
* integer: http status code
* string: response-body
* false: http status 404

## respond-type
`respond-type` is like `respond` but sets the content-type header based on a symbol identifier.
type identifiers supported by default are: `json`, `html`, `js`, `css`, `text`.
the hashtable `swa-http-key->mime-type` from `(sph web app http)` defines the types and can be extended

examples
```
(respond-type (quote json) "{\"test\": 1}")
(respond-type (quote text) 200 (list) "test")
```

signature
```
symbol:content-type-identifier respond-argument ... -> swa-http-response
```

# client code and assets
* code to be executed by the client can be created directly where needed in scheme procedures or loaded from external files under ``client/{format-name}/``
* source formats recognised by default: html, css, javascript, sxml, plcss, sescript
* target formats supported by default: html, css, javascript
* import `(sph web app client)` to use the following features

## configuration options
```
client-output-path "webroot/"
web-base-path "/"
```

## static files
static files are either in the web root and handled by a proxy webserver (possibly using features like x-accel-redirect), or read and send by the application.
for compiled static files, create a `client-static` config object and define file bundles with `client-static-config-create`

```
(import (sph web app client))

(define client-static-config
  (client-static-config-create
    (default
      js ("main")
      css ((example-variable "testvalue" another-variable 3) "main" "otherfile"))
    (otherbundle js ("otherfile"))))
```

sources are specified as for `client-file` and paths are used like this `{swa-root}/client/{output-format}/{relative-path}`. all available supported formats can be used, for example also plain html and html from sxml. and the source files can also be sjs or plcss, with the file path `client/css/main.plccs` for example

in `app-init`, call
```
(client-static-compile swa-env client-static-config)
```

where needed, get the public path to one or multiple compiled bundle files with `client-static`
```
(client-static swa-env (quote css) (quote (bundle-name other-bundle-name)))
```

## dynamic files
``(sph web app client)`` has exports to pre-process files/templates and sets of those on demand

template variables, which are passed in association lists with symbol keys to client file processors, are made available in unquote in the template code

### client-file
client-file writes the eventually processed result to a file with an automatically generated file name with an underscore prefix, unless set otherwise using the file-name parameter.
paths are relative to ``{swa-root}/client/{output-format}/``

```
(client-file swa-env output-format template-variables file-path #:optional file-name)
(client-file swa-env (quote js) (quote ((a . 2) (b . 3))) "relative-path")
```

### client-port
like client-file but writes output to a port

```
(client-port swa-env output-format port-output bindings sources)
```

## template files
both the static and dynamic `client-` bindings read s-expression based formats like sxml, plcss and sescript as templates.
templates are interpreted as if being quasiquoted, and unquote can be used to insert results of scheme expressions and access template variables.

example file content with insert of a template variable `a`
```
(div (@ (class test)) (unquote a))
```
also
```
(div (@ (class test)) ,a)
```

example 2
```
(div
  (unquote
    (if example-variable
      (qq (span "a"))
      (qq (span (@ (class test-class)) "b")))))
```

## change file processors
modify the `client-ac-config` hashtable, which maps `output-format-symbol` to `(ac-output ac-input ...)`, see also `modules/sph/filesystem/asset-compiler.scm` and `client.scm`

example
```
(import (sph filesystem asset-compiler) (sph web app client) (rnrs hashtables))

(hashtable-set! client-ac-config (quote tar)
  (list
    (ac-config-output (quote tar) ".tar" ac-output-copy)
    (ac-config-input (quote tgz) ".tgz"
      (lambda (source next-port options)
        (call-with-input-file source (lambda (file) (decompress file next-port)))))))
```

# deriving from projects
import application modules as scheme libraries where needed

for example in modules/sph-mn.scm
```
(import (sph web app) (prefix (sph-cms) cms-))

(define (app-init swa-env)
  ; calls the init routine of the other project to use its special features
  ((swa-app-init cms-swa-app) swa-env))

(define (app-respond request)
  (respond "i'm derived"))

(define swa-app
 (swa-app-new app-respond
    #:init app-init #:deinit (swa-app-deinit cms-swa-app)))
```

this imports the swa-app object from (sph-cms) with the identifier prefixed with cms- to avoid a name conflict in the current module, then uses the swa-app object accessor swa-app-init to retrieve the other app-init procedure and calls it with the swa-env. all client files that are compiled are saved in the current/parent swa-root

# proxy setup
a proxy is not necessary if instead of swa-server-scgi, swa-server-guile is used. however, scgi is the recommended way because scgi is an interface that allows an established http server to do the work of http request parsing and connection management - and there are servers that do this well, stable and fast.
for example nginx allows for fast file transfers without redirection through the application (x-accel), https, keep-alive requests, chunked encoding, gzip compression, load-balancing and much more. to do this, set up a server to interface with the socket that is created when starting the application with the scgi server (the path is displayed when the server starts). nginx, apache and lighttp support it

see [nginx example configuration for scgi and web-app](nginx.md)

# deployment
the target host needs to have guile and sph-web-app set up. setup is the same as for local development environments

example systemd service file `/etc/systemd/system/sph-info.service`
```
[Unit]
Description=sph-info scgi application

[Service]
Environment="GUILE_LOAD_PATH=/usr/share/guile/2.2:/usr/share/guile/site"
WorkingDirectory=/opt/sph-info
ExecStart=/opt/sph-info/exe/start-dynamic
Type=simple
Restart=on-failure
User=sph-info

[Install]
WantedBy=multi-user.target
```

# examples
example project directory with client, config, webroot and custom other directories
```
exe
  start
client
  css
    main.css
  js
    main.js
config
  default
  development
modules
  project-name.scm
readme.md
webroot
  assets
    css/_main.css
    js/_main.js
```

* webroot: the only directory directly accessible for web server clients. the document root for a (proxy) web server. contains compiled client code files under assets and any static files that should be accessible to clients without redirection through the application. the name "root" was chosen in favor of "public" or "shared"
* exe: contains executable files for general project management. for example for starting the application or custom helper scripts. the name "exe" was chosen in favor of "bin" or "script"
* client: the client directory contains code or sources and templates for code that is to be evaluated by the client. for example html, css or javascript

example content for a file `exe/start`
```
#!/usr/bin/guile
!#

(add-to-load-path (string-append (getcwd) "/modules"))
(import (sph) (sph web app) (sph-info))

(apply
  (l* (#:optional (config "production"))
      (swa-start swa-app (getcwd) config swa-server-scgi #:parse-query #t))
        (tail (program-arguments)))
```

example `modules/project-name.scm`

```
(library (project-name)
  (export swa-app)
  (import
    (rnrs base)
    (sph web app)
    (sph web app http))

  (define (app-respond request)
    (respond "test"))

  (define swa-app (swa-app-new app-respond)))
```

start the application
```
./exe/start
```
