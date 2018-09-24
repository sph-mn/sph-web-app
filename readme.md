small but scheme-typically powerful web application framework

* initialises web applications, starts a server and provides optional modules for some features commonly needed for web projects
* similar perhaps to express.js
* status: should work. maintained

# minimal example

```scheme
(import (sph web app) (sph web app http))

(define (app-respond request) (respond "test"))

(define swa-app (swa-create (quote project-name) app-respond))

(swa-start swa-app #f swa-server-guile)
```

put the above in a file ``example.scm`` then

```shell
guile example.scm
```

see

```
listening on 127.0.0.1:6500
exit with ctrl+c
```

then take a browser and go to

```
http://127.0.0.1:6500
```

# features
* starts a server, passes request objects to handler procedures and transmits response objects
* protocol agnostic core: socket -> web-app -> socket
* pluggable server (thread-pool scgi, fibers scgi, direct http, none for testing ...)
* composable projects. projects can derive functionality and assets from other projects
* derivative environment configuration files in an s-expression format
* mostly functional, avoids side-effects. no "set!" used
* basic routing on any request property and url pattern matching
* the scgi servers are for use with a proxy like nginx for https, automatic chunked-encoding, fast file transfers, keep-alive, websockets, load-balancing, caching, general http robustness and more
* optional modules
  * http requests/responses
  * templating with s-expression versions of [xml (sxml)]("https://en.wikipedia.org/wiki/SXML"), css [(plcss)](http://sph.mn/c/browse/link-view/sph-lang-plcss/library/documentation) and javascript [(sescript)](https://github.com/sph-mn/sescript)
  * asset processing: transcompilation, bundling, minification, compression, formatting, etc
* fast
  * plain vectors as records for request and response objects
  * response objects can have procedures that send data while it is generated
  * requests have little overhead as little data has to be prepared

# documentation

[web-app manual](other/documentation/manual.md)

## other learning resources
an example project can be created with
```shell
web-app --example project-name
```

sourcecode of live projects
* [smaller project that does not use a database](http://files.sph.mn/sourcecode/ytilitu)
* [a content management system](http://files.sph.mn/sourcecode/sph-cms)

# dependencies
* [guile](https://www.gnu.org/software/guile/guile.html)
* [sph-lib](https://github.com/sph-mn/sph-lib)
* optional
  * [sescript](https://github.com/sph-mn/sescript)

# installation
* install all dependencies if there are some
* [download](http://files.sph.mn/u/software/releases)
  * alternatives: [git clone](https://github.com/sph-mn/sph-web-app), [sph.mn](http://sph.mn/git/download/sph-web-app.stable.tgz)
* unpack the downloaded archive. for example with "tar -xf sph-web-app.tgz" (-x is for extract, -f is for the input file)

```shell
cd sph-web-app
su root
./exe/install
```

the installer copies files and sets their filesystem permissions. the script can take one argument, the path prefix to install to

## pacman package
using [aurget](https://github.com/pbrisbin/aurget)

```shell
aurget -S --deps sph-web-app-git
```

# related
* [(sph lang template)](http://sph.mn/c/view/q6)
* [(sph scgi)](http://sph.mn/c/view/m6)
* [(sph lang plcss)](http://sph.mn/c/view/fq)
* [sxml](http://okmij.org/ftp/Scheme/SXML.html)
