;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat active)
  (export format load syntax->location)
  (import (rnrs base (6))
          (only (chezscheme)
                annotation-source format load locate-source-object-source
                syntax->annotation))

  (define (syntax->location s)
    (locate-source-object-source
      (annotation-source (syntax->annotation s))
      #t    ; get the start, not end
      #t))) ; use the cache
