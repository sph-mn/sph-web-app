see [sph.mn](http://sph.mn/c/view/mu) for full documentation.

# minimal example

```
(import (sph web app) (sph web app http))

(define (app-respond request) (respond "test"))

(define swa-app (swa-create (quote project-name) app-respond))

(swa-start swa-app #f swa-server-guile)
```

put the above in a file "example.scm" then

```
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
