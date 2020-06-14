;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src utils)
  (export ->string format)
  (import (rnrs (6))
          ; (only (racket base) get-output-string open-output-string)
          (rnrs control (6))
          (rnrs mutable-strings (6))
          (rnrs io simple (6)))

  ;; Converts `x` to a string using `display`.
  (define (->string x)
    x)
    ; (let ((out (open-output-string)))
    ;   (display x out)
    ;   (let ((str (get-output-string out)))
    ;     (close-output-port out)
    ;     str)))

  ;; Formats a string by replacing occurrences of "%" with `args` in order.
  (define (format template . args)
    (define (set-substring! dst index src)
      (do ((i 0 (+ i 1)))
          ((= i (string-length src)))
        (string-set! dst (+ index i) (string-ref src i))))
    (let* ((template-len (length template))
           (strings (map ->string args))
           (total-len (+ template-len
                         (fold-left + 0 (map string-length strings))
                         (- (length args))))
           (out (make-string total-len)))
      (let loop ((i 0) (j 0) (strs strings))
        (cond
          ((= i template-len)
           (if (null? strs)
               out
               (error 'format "too few % characters in template string")))
          ((char=? (string-ref template i) #\%)
           (if (null? strs)
               (error 'format "too many % characters in template string")
               (begin
                 (set-substring! out j (car strs))
                 (loop (+ i 1) (+ j (string-length (car strs))) (cdr strs)))))
          (else
           (string-set! out j (string-ref template i))
           (loop (+ i 1) (+ j 1) strs)))))))
