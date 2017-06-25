(library (project-example lib main)
  (export
    helper)
  (import
    (sph base))

  ; for code shared between branches or any other part of the code.

  (define (helper) #t))
