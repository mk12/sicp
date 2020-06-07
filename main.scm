#!/usr/bin/env chez --script

(let*
  ((argv (command-line))
   (program (car argv))
   (arguments (cdr argv))
   (usage-message
    (format "usage: ~s [CHAPTER]\n" program))
   (chapters '(1 2))
   (fail
     (lambda ()
       (display usage-message
                (standard-error-port 'none (current-transcoder)))
       (exit 1)))
   (chapter
    (case (length arguments)
      ((0) 'all)
      ((1) (case (car arguments)
             (("-h" "--help")
              (display usage-message)
              (exit 0))
             (("1" "2" "3")
              (string->number (car arguments)))
             (else (fail))))
      (else (fail))))
   (check?
     (lambda (ch)
       (or (eq? 'all chapter) (= ch chapter)))))
  (load "syntax.scm")
  (load "chapter-1.scm")
  (load "chapter-2.scm")
  (when (check? 1))
     )
       )
     )))

(let ()
  (define argv (command-line))
  (define program (car argv))
  (define arguments (cdr argv))
  (define usage-message
    (format "usage: ~s [CHAPTER]\n" program))
  (define (fail)
    (display usage-message
             (standard-error-port 'none (current-transcoder)))
    (exit 1))
  (define chapter
    (case (length arguments)
      ((0) 'all)
      ((1) (case (car arguments)
             (("-h" "--help")
              (display usage-message)
              (exit 0))
             (("1" "2" "3")
              (string->number (car arguments)))
             (else (fail))))
      (else (fail))))
  (load "prelude.scm")
  (load "chapter-1.scm")
  (load "chapter-2.scm")
  )

(main (command-line))
