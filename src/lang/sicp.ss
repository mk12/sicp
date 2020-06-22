;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src lang sicp)
  (export SICP Chapter Section Subsection Exercise define => ~> slow=> slow~>
          capture-output hide-output)
  (import (src lang core)))
