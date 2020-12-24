;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src sicp chapter-5)
  (export chapter-5-effects)
  (import (rnrs base (6))
          (src lang sicp)
          (only (src sicp chapter-4) chapter-4-effects))

;; Introduce a dependency on the previous chapter so that it executes first.
(define chapter-5-effects chapter-4-effects)

(SICP

(Chapter :4 "Computing with Register Machines")

) ; end of SICP
) ; end of library
