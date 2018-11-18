small but scheme-typically powerful web application framework

* initialises web applications, starts a server and provides optional modules for some features commonly needed for web projects
* similar perhaps to express.js
* status: should work. maintained as of 2018-11

# minimal example
```scheme
(import (sph web app) (sph web app http))

(define (app-respond request) (respond "test"))

(define swa-app (swa-app-new app-respond))

(swa-start swa-app #f #f swa-server-guile)
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
* reads configuration files, starts a server, passes request objects to handler procedures and transmits response objects
* protocol agnostic core: socket -> web-app -> socket
* pluggable server (thread-pool scgi, fibers scgi, direct http, none for testing ...)
* derivative environment configuration files in an s-expression format
* mostly functional, avoids side-effects. no "set!" used
* basic routing on any request property and url pattern matching
* the scgi servers are for use with a proxy like nginx for https, automatic chunked-encoding, fast file transfers, keep-alive, websockets, load-balancing, caching, general http robustness and more
* optional modules
  * http requests/responses
  * templating with s-expression versions of [xml (sxml)]("https://en.wikipedia.org/wiki/SXML"), css [(plcss)](http://sph.mn/computer/software/sph-lib/plcss.html) and javascript [(sescript)](https://github.com/sph-mn/sescript)
  * asset processing: transcompilation, bundling, minification, compression, formatting, etc
* fast
  * plain vectors as records for request and response objects
  * response objects can have procedures that send data while it is generated
  * requests have little overhead as little data has to be prepared

# documentation
[web-app manual](other/documentation/manual.md)

# dependencies
* [guile](https://www.gnu.org/software/guile/guile.html)
* [sph-lib](https://github.com/sph-mn/sph-lib)
* optional
  * [sescript](https://github.com/sph-mn/sescript) (for (sph web app client))

# installation
* install all dependencies
* [download](http://files.sph.mn/u/software/releases)
  * alternatives: [git clone](https://github.com/sph-mn/sph-web-app)
* unpack the downloaded archive. for example with "tar -xf sph-web-app.tgz" (-x for extract, -f for the input file)

```shell
cd sph-web-app
su root
./exe/install
```

the installer is a shell script that copies files and sets filesystem permissions. the script can take one argument, the path prefix to install to

## pacman package
for example using [aurget](https://github.com/pbrisbin/aurget)

```shell
aurget -S --deps sph-web-app-git
```
