#!/usr/bin/env chez --script

(module (main)
  (define (usage program)
    (format "\
usage: ~A [-hsv] [CHAPTER]

Tests code samples and exercises from SICP.
If CHAPTER is omitted, runs all chapters.

options:

  -h, --help       show this help message
  -s, --slow       run slow tests too
  -v, --verbose    verbose output
" program))

  (define (main argv)
    (let ((program (car argv))
          (options (parse-options (cdr argv))))
      (cond
        ((assq 'error options)
         (die (usage program)))
        ((assq 'help options)
         (display (usage program))
         (exit 0))
        (else
          (run (assq 'chapter options)
               (assq 'slow options)
               (assq 'verbose options))))))

  (define (die msg)
    (display msg (standard-error-port 'none (current-transcoder)))
    (exit 1))

  (define (parse-options args)
    (define (go args options)
      (if (null? args)
          options
          (case (car args)
            (("-h" "--help") '((help)))
            (("-s" "--slow")
             (go (cdr args) (cons '(slow) options)))
            (("-v" "--verbose")
             (go (cdr args) (cons '(verbose) options)))
            (("1" "2" "3")
             (go (cdr args) (cons `(chapter ,(string->number (car args)))
                                  options)))
            (else '((error))))))
    (go args '((chapter all))))
  
  (define (run chapter slow verbose)
    (load "syntax.scm")
    (load "chapter-1.scm")
    ; TODO
    )
  )

(main (command-line))
