;;; Copyright 2021 Mitchell Kember. Subject to the CC BY-SA 4.0 License.

#!r6rs

(library (src sicp chapter-5)
  (export chapter-5-effects)
  (import (rnrs base (6))
          (src lang sicp)
          (only (src sicp chapter-4) chapter-4-effects))

;; Introduce a dependency on the previous chapter so that it executes first.
(define chapter-5-effects chapter-4-effects)

(SICP

(Chapter :5 "Computing with Register Machines")

(Section :5.1 "Designing Register Machines")

(Exercise ?5.1)

(Exercise ?5.6)

(Section :5.2 "A Register-Machine Simulator")

(Exercise ?5.7)

(Exercise ?5.19)

(Section :5.3 "Storage Allocation and Garbage Collection")

(Exercise ?5.20)

(Exercise ?5.22)

(Section :5.4 "The Explicit-Control Evaluator")

(Exercise ?5.23)

(Exercise ?5.30)

(Section :5.5 "Compilation")

(Exercise ?5.31)

(Exercise ?5.52)

) ; end of SICP
) ; end of library
