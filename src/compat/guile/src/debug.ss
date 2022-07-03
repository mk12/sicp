;;; Copyright 2022 Mitchell Kember. Subject to the MIT License.

;; This file is meant to be loaded with the `-l` option to guile before the main
;; program, not imported as a module.

(define (r) (load "../../../main.ss"))

(display "\x1b;[31;1mNOTE: Start debugging with (r)\x1b;[0m\n\n")
