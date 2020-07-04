;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat active)
  (export current-output-port format make-parameter open-output-string
          parameterize random runtime seed-rng string-contains? syntax->location
          with-output-to-string)
  (import (rnrs base (6))
          (only (chezscheme)
                annotation-source current-output-port current-time format
                locate-source-object-source make-parameter open-output-string
                parameterize random random-seed syntax->annotation
                time-nanosecond time-second with-output-to-string))

  (define (runtime)
    (let ((t (current-time 'time-monotonic)))
      (+ (time-second t)
         (/ (time-nanosecond t) 1e9))))

  (define (seed-rng)
    (random-seed
      (+ 1 (mod (time-second (current-time))
                (- (expt 2 32) 1)))))

  ;; Rabin-Karp string search algorithm. Assumes ASCII.
  (define (string-contains? s1 s2)
    (define byte-base 256)
    (define prime-modulus 101)
    (define (mult x) (* x byte-base))
    (define (modp x) (mod x prime-modulus))
    (define (hash s len)
      (define (iter h i)
        (if (= i len)
            h
            (iter (modp (+ (modp (mult h))
                           (char->integer (string-ref s i))))
                  (+ i 1))))
      (iter 0 0))
    (define (roll h msd-value remove add)
      (modp (+ (mult (- (+ h prime-modulus)
                        (modp (* (char->integer remove) msd-value))))
               (char->integer add))))
    (define (calc-msd-value)
      (let loop ((i (string-length s2))
                 (val 1))
        (if (= i 1)
            val
            (loop (- i 1) (modp (mult val))))))
    (define (substring=? s1 i1 s2 i2 len)
      (define (iter offset)
        (cond
          ((= offset len) #t)
          ((not (char=? (string-ref s1 (+ i1 offset))
                        (string-ref s2 (+ i2 offset)))) #f)
          (else (iter (+ offset 1)))))
      (cond
        ((< (string-length s1) len) #f)
        ((< (string-length s2) len) #f)
        (else (iter 0))))
    (let ((n (string-length s1))
          (m (string-length s2)))
      (cond
        ((< n m) #f)
        ((zero? m) #t)
        (else
          (let ((end (- n m))
                (hs2 (hash s2 m))
                (msd-value (calc-msd-value)))
            (let loop ((i 0)
                       (h (hash s1 m)))
              (cond
                ((and (= h hs2) (substring=? s1 i s2 0 m)) #t)
                ((= i end) #f)
                (else
                  (loop (+ i 1)
                        (roll
                          h
                          msd-value
                          (string-ref s1 i)
                          (string-ref s1 (+ i m))))))))))))

  (define (syntax->location s)
    (locate-source-object-source
      (annotation-source (syntax->annotation s))
      #t    ; get the start, not end
      #t))) ; use the cache
