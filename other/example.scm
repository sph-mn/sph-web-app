(import (sph web app) (sph web app http))

(define (app-respond request) (respond "test"))

(define swa-app (swa-create (quote project-name) app-respond))

(swa-start swa-app #f swa-server-guile)
