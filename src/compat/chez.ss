;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat active)
  (export current-output-port format open-output-string parameterize
          syntax->location with-output-to-string)
  (import (rnrs base (6))
          (only (chezscheme)
                annotation-source current-output-port format
                locate-source-object-source open-output-string parameterize
                syntax->annotation with-output-to-string))

  (define (syntax->location s)
    (locate-source-object-source
      (annotation-source (syntax->annotation s))
      #t    ; get the start, not end
      #t))) ; use the cache
