;;; Copyright 2022 Mitchell Kember. Subject to the MIT License.

;; This file is meant to be loaded with the `-l` option to guile before the main
;; program, not imported as a module.

(use-modules ((system base compile)
              #:select (default-optimization-level)))

;; By default, Guile auto-compiles with -O2. This takes a really long time for
;; this project, 10x longer! And it doesn't pay off because the tests run fairly
;; quickly without optimization. We choose -O1 because it compiles just as fast
;; as -O0, and results in the best overall (fresh compile + run) time.
(default-optimization-level 1)
