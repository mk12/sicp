;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat common)
  (export define-syntax)
  (import (only (rnrs base (6)) lambda syntax-rules)
          (prefix (only (rnrs base (6)) define-syntax) rnrs-))

  (rnrs-define-syntax define-syntax
    (syntax-rules ()
      ((_ (name x) rest ...)
       (rnrs-define-syntax name
         (lambda (x) rest ...)))
      ((_ e ...)
       (rnrs-define-syntax e ...)))))
