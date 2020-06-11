(define (assert-equal e1 e2 v1 v2)
  (unless (equal? v1 v2)
    (syntax-error
      e1
      (format
        (string-append
          "failed!"
          "\n\nleft: ~s\n => ~s"
          "\n\nright: ~s\n => ~s\n\n")
          (syntax->datum e1) v1 (syntax->datum e2) v2))))

(define-syntax ae
  (syntax-rules ()
    ((_ e1 e2)
     (let ((v1 e1) (v2 e2))
      (assert-equal #'e1 #'e2 v1 v2)))))

(ae

  (+ 1 1)


    (* 2 3))
