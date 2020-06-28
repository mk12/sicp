;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src lang core)
  (export SICP Chapter Section Subsection Exercise define => ~> slow=> slow~>
          run-sicp)
  (import (except (rnrs (6)) current-output-port)
          (rnrs mutable-pairs (6))
          (src compat active))

  ;; Global flag for whether to use ANSI color in output.
  (define *color* #f)

  ;; Returns `str` with ANSI escape codes to make it `color`, if `*color*` is
  ;; set. Otherwise, returns `str` unchanged.
  (define (ansi color str)
    (if *color*
      (let ((code (case color
                    ((bold) "1")
                    ((bold-red) "1;31")
                    ((green) "32")
                    ((yellow) "33")
                    ((blue) "34"))))
        (string-append "\x1b;[" code "m" str "\x1b;[0m"))
      str))

  ;; Global test counters, used for reporting and for setting the exit status.
  ;; When running without a filter, total = passes + fails + skips. Otherwise,
  ;; the difference is equal to the number of tests filtered out.
  (define *total* 0)
  (define *passes* 0)
  (define *fails* 0)
  (define *skips* 0)

  ;; Increases `*total*` by `n` tests. We need a separate function for this
  ;; because using `*total*` in macros would indirectly export it, meaning it
  ;; would have to be immutable.
  (define (increase-total-tests! n)
    (set! *total* (+ *total* n)))

  ;; Asserts that all elements in `vals` are the same, according to `equal?`.
  ;; Expects `exprs` to contain syntax objects for each value in `vals`.
  (define (assert-equal vals exprs)
    (assert-on-pairs
      equal?
      (lambda (v1 v2 e1 e2)
        (format
          (string-append
            "left: " (ansi 'blue "~s") "\n=> " (ansi 'green "~s") "\n\n"
            "right: " (ansi 'blue "~s") "\n=> " (ansi 'green "~s") "\n\n")
          e1 v1 e2 v2))
      vals
      exprs))

  ;; Asserts that each number in `vals` is very close to the previous one.
  ;; Expects `exprs` to contain syntax objects for each value in `vals`.
  (define (assert-close vals exprs)
    (define (delta x y) (abs (- x y)))
    (define max-delta 1e-10)
    (assert-on-pairs
      (lambda (v1 v2)
        (<= (delta v1 v2) max-delta))
      (lambda (v1 v2 e1 e2)
        (format
          (string-append
            "left: " (ansi 'blue "~s") "\n=> " (ansi 'green "~s") "\n\n"
            "right: " (ansi 'blue "~s") "\n=> " (ansi 'green "~s") "\n\n"
            "delta: " (ansi 'green "~s") " > ~s\n\n")
          e1 v1 e2 v2 (delta v1 v2) max-delta))
      vals
      exprs))

  ;; Asserts that `pred?` holds for each pair of adjacent elements in `vals`.
  ;; Expects `exprs` to contain syntax objects for each value in `vals`. If
  ;; `(pred? v1 v2)` is false, the error message includes the `v1` line number
  ;; and the string `(make-msg v1 v2 d1 d2)`, where `d1` and `d2` are the
  ;; corresponding unevaluated forms as data (not syntax objects). Also
  ;; increments `*passes*` or `*fails*` based on the result.
  (define (assert-on-pairs pred? make-msg vals exprs)
    (for-each
      (lambda (vpair epair)
        (let ((v1 (car vpair))
              (v2 (cdr vpair))
              (e1 (car epair))
              (e2 (cdr epair)))
          (cond
            ((pred? v1 v2) (set! *passes* (+ *passes* 1)))
            (else
              (set! *fails* (+ *fails* 1))
              (let-values (((file line col) (syntax->location e1)))
                (let ((msg (make-msg
                            v1 v2 (syntax->datum e1) (syntax->datum e2))))
                  (display
                    (format
                      (string-append
                        (ansi 'bold "~a:~a:~a: assertion failed")
                        "\n~a")
                      file line col msg))))))))
      (pairs vals)
      (pairs (syntax->list exprs))))

  ;; Converts a syntax object to a list of syntax objects.
  ;; https://www.scheme.com/csug8/syntax.html#./syntax:s6
  (define syntax->list
    (lambda (ls)
      (syntax-case ls ()
        (() '())
        ((x . r) (cons #'x (syntax->list #'r)))))) 

  ;; Returns a list of the adjacent pairs of elements in `xs` For example, given
  ;; the list `(a b c d)` it returns `((a . b) (b . c) (c . d))`.
  (define (pairs xs)
    (cond
      ((null? xs) '())
      ((null? (cdr xs)) '())
      (else (cons (cons (car xs) (cadr xs))
                  (pairs (cdr xs))))))

  ;; Global flag indicating whether to run slow tests.
  (define *slow* #f)

  ;; Returns whether to run slow tests. We need a separate function for this
  ;; because using `*slow*` in macros would indirectly export it, meaning it
  ;; would have to be immutable.
  (define (slow-enabled?) *slow*)

  ;; Increments the `*skips*` counter for a skipped slow test.
  (define (skip-slow-test)
    (set! *skips* (+ *skips* 1)))

  ;; Wraps code so that it only executes when `*slow*` is true, and otherwise
  ;; increments the `*skips*` counter.
  (define-syntax when-slow
    (syntax-rules ()
      ((_ e) (if (slow-enabled?) e (skip-slow-test)))))

  ;; An entry stores code from a part of the textbook. It consists of a unique
  ;; symbol `id`, a kind ('chapter, 'section, 'subsection, or 'exercise), a
  ;; string `num` containing a dotted chapter/section/etc. number like "1.2.3",
  ;; a title string (or #f), a list of imported names from other entries
  ;; formatted as `((id name ...) ...)`, a list of exported names, and a thunk
  ;; that takes all the imported names as one flat list of arguments and returns
  ;; the exported values in the same order as `exports`.
  (define-record-type entry
    (fields id kind num title imports exports thunk))

  ;; A queue supports constant time appending to the back, popping from the
  ;; front, and accessing the length.
  (define-record-type (queue make-raw-queue queue?)
    (fields (mutable length) (mutable front) (mutable back)))
  (define (make-queue)
    (make-raw-queue 0 '() '()))
  (define (queue-empty? q)
    (null? (queue-front q)))
  (define (queue-push-back! q x)
    (let ((new-pair (cons x '())))
      (queue-length-set! q (+ (queue-length q) 1))
      (cond
        ((queue-empty? q)
         (queue-front-set! q new-pair)
         (queue-back-set! q new-pair))
        (else
          (set-cdr! (queue-back q) new-pair)
          (queue-back-set! q new-pair)))))
  (define (queue-pop-front! q)
    (if (queue-empty? q)
        (error 'queue-pop-front! "empty queue")
        (let ((result (car (queue-front q))))
          (cond
            ((eq? (queue-front q) (queue-back q))
             (queue-front-set! q '())
             (queue-back-set! q '()))
            (else
              (queue-front-set! q (cdr (queue-front q)))))
          result)))

  ;; Global queue of entries. The `SICP` macro produces calls to `add-entry!`.
  (define *entries* (make-queue))
  (define (add-entry! id kind num title imports exports thunk)
    (queue-push-back! *entries*
                      (make-entry id kind num title imports exports thunk)))

  ;; Returns a hashtable from `id` to objects in `*entries*`.
  (define (entries-by-id)
    (define table (make-eq-hashtable (queue-length *entries*)))
    (for-each
      (lambda (entry)
        (hashtable-set! table (entry-id entry) entry))
      (queue-front *entries*))
    table)

  ;; Returns a hashtable from entries in `*entries*` to their in-degrees (the
  ;; number of times they are imported). Includes only entries that satisfy
  ;; `pred?` and all their transitive dependencies. Requires `by-id`, a
  ;; hashtable produced by `entries-by-id`.
  (define (entries-to-in-degrees by-id pred?)
    (define in-degrees (make-eq-hashtable))
    (define q (make-queue))
    (for-each
      (lambda (entry)
        (when (pred? entry)
          (queue-push-back! q entry)))
      (queue-front *entries*))
    (let loop ()
      (if (queue-empty? q)
          in-degrees
          (let ((e (queue-pop-front! q)))
            (let ((deps (map (lambda (import-list)
                               (hashtable-ref by-id (car import-list) #f))
                             (entry-imports e))))
              (if (hashtable-contains? in-degrees e)
                  (hashtable-update! in-degrees e (lambda (x) (+ x 1)) #f)
                  (begin
                    (hashtable-set! in-degrees e 0)
                    (for-each
                      (lambda (d)
                        (queue-push-back! q d))
                      deps)))
              (loop))))))

  ;; Topologically sorts entries from `*entries*`, including only those that
  ;; satisfy `pred?` and all their transitive dependencies. Requires `by-id`, a
  ;; hashtable produced by `entries-by-id`. Raises an error if sorting fails
  ;; because of a cycle.
  (define (sort-entries by-id pred?)
    (define in-degrees (entries-to-in-degrees by-id pred?))
    (define sources (make-queue))
    (define sorted '())
    (for-each
      (lambda (entry)
        (when (zero? (hashtable-ref in-degrees entry -1))
          (queue-push-back! sources entry)))
      (reverse (queue-front *entries*)))
    (let loop ()
      (unless (queue-empty? sources)
        (let ((node (queue-pop-front! sources)))
          (set! sorted (cons node sorted))
          (for-each
            (lambda (import-list)
              (let* ((e (hashtable-ref by-id (car import-list) #f))
                     (deg (hashtable-ref in-degrees e #f))
                     (new-deg (- deg 1)))
                (hashtable-set! in-degrees e new-deg)
                (when (zero? new-deg)
                  (queue-push-back! sources e))))
            (entry-imports node))
          (loop))))
    (let-values (((entries degrees) (hashtable-entries in-degrees)))
      (vector-for-each
        (lambda (e deg)
          (unless (zero? deg)
            (error 'sort-entries "import cycle in entries" (entry-id e))))
        entries
        degrees))
    sorted)

  ;; Executes the code in `*entries*`. If `slow` is true, runs slow tests. If
  ;; `verbose` is true, prints more verbose information about all tests. If
  ;; `color` is true, prints color output. If `filters` is nonempty, only runs
  ;; entries whose `id` matches at least one of the items in `filters` (and all
  ;; their transitive depedencies). Returns #t if all tests passed.
  (define (run-sicp filters slow verbose color)
    (define (include-entry? entry)
      (define (match? s)
        (let ((s-len (string-length s)))
          (and (> s-len 0)
               (let* ((sigil (memv (string-ref s 0) '(#\: #\?)))
                      (e (if sigil
                             (symbol->string (entry-id entry))
                             (entry-num entry))))
                 (or (string=? s e)
                     (and sigil (= s-len 1) (string=? s (substring e 0 1)))
                     (and (< s-len (string-length e))
                          (string=? s (substring e 0 s-len))
                          (char=? #\. (string-ref e s-len))))))))
      (or (null? filters)
          (exists match? filters)))
    (define by-id (entries-by-id))
    (define sorted (sort-entries by-id include-entry?))
    (define results (make-eq-hashtable))
    (define (gather-args importer)
      (define (gather import-list)
        (let* ((exporter (hashtable-ref by-id (car import-list) #f))
               (import-names (cdr import-list))
               (export-names (entry-exports exporter))
               (export-values (hashtable-ref results exporter #f)))
          (define (find-value name)
            (let loop ((en export-names)
                       (ev export-values))
              (if (or (null? en) (null? ev))
                  (error 'run-sicp
                         (format "~a imports nonexistent (~a ~a)"
                                 (entry-id importer)
                                 name
                                 (entry-id exporter))))))
          (map find-value import-names)))
      (fold-left append '() (map gather (entry-imports importer))))
    (when slow (set! *slow* #t))
    (when color (set! *color* #t))
    (for-each
      (lambda (e)
        (when verbose
          (display
            (ansi 'yellow
                  (format "* ~a ~a~a\n"
                          (string-titlecase (symbol->string (entry-kind e)))
                          (entry-num e)
                          (if (entry-title e)
                            (string-append ": " (entry-title e))
                            "")))))
        (hashtable-set!
          results
          e
          (apply (entry-thunk e) (gather-args e))))
      sorted)
    (when verbose (newline))
    (display
      (format
        "test result: ~a. ~a passed; ~a failed; ~a skipped; ~a filtered out\n"
        (if (zero? *fails*) (ansi 'green "ok") (ansi 'bold-red "FAIL"))
        *passes* *fails* *skips* (- *total* *passes* *fails* *skips*)))
    (zero? *fails*)
    ;; PRINT INFO
    ; (write filters) (newline)
    ; (write slow) (newline)
    ; (write verbose) (newline)
    ; (write *entries*) (newline)
    ; (write (map entry-id sorted))
    )

  ;; In order for `SICP` to match on auxiliary keywords, we must export them.
  ;; That implies giving them definitions. We don't need to do this for `define`
  ;; and `=>` because we instead re-export the rnrs definitions (the latter is
  ;; already an auxiliary keyword used in `cond`).
  (let-syntax
    ((auxiliary
       (syntax-rules ()
         ((_ lit ...)
          (begin
            (define-syntax lit
              (lambda (x)
                (syntax-violation #f "incorrect usage of auxiliary keyword" x)))
            ...)))))
    (auxiliary Chapter Section Subsection Exercise ~> slow=> slow~>))

  ;; A DSL for SICP code samples and exercises.
  (define-syntax SICP (lambda (x)
    ;; Use the `SICP` syntax form in `datum->syntax` calls.
    (define sicp (with-syntax (((s e* ...) x)) #'s))

    ;; Creates an entry `num` from its `id` syntax. This just chops off the
    ;; sigil used to differentiate chapters/sections/subsections from exercises
    ;; (and also used because `1.1.1` on its own is an invalid R6RS identifier).
    (define (entry-id->num id)
      (let ((str (symbol->string (syntax->datum id))))
        (substring str 1 (string-length str))))

    ;; Recursive implementation of the macro. It takes:
    ;; x       - the remaining forms to be processed
    ;; header  - the last seen chapter/section/subsection/exercise header
    ;; exports - names of definitions made since the last header
    ;; body    - code encountered since the last header
    ;; ntests  - number of tests/asserted processed so far
    ;; out     - accumulated result of the macro
    (define (go x header exports body ntests out)
      (define (flush)
          (if (eq? header 'no-header)
            out
            #`(#,@out #,(build-entry))))
      (define (build-entry)
        (with-syntax (((kind id _ ...) header)
                      (((import-id import-name ...) ...) (get-uses))
                      ((export-name ...) exports))
              ;; PRINT THE THUNK
              ; (write (syntax->datum #`(lambda (import-name ... ...)
              ;   (define export-name) ...
              ;   #,@body
              ;   (list export-name ...))))
              ; (newline)(newline)
          #`(add-entry!
              'id
              'kind
              #,(entry-id->num #'id)
              #,(get-title)
              '((import-id import-name ...) ...)
              '#,exports
              (lambda (import-name ... ...)
                (define export-name) ...
                #,@body
                (list export-name ...)))))
      (define (get-title)
        (syntax-case header ()
          ((_ _ title _ ...) (string? (syntax->datum #'title)) #'title)
          (_ #'#f)))
      (define (get-uses)
        (syntax-case header (use)
          ((_ _ (use e* ...)) #'(e* ...))
          ((_ _ _ (use e* ...)) #'(e* ...))
          (_ '())))
      (define (add-export name)
        (if (memq (syntax->datum name)
                  (syntax->datum exports))
          exports
          #`(#,@exports #,name)))
      (syntax-case x (Chapter Section Subsection Exercise
                      define => ~> slow=> slow~>)
        (() #`(#,@(flush) (increase-total-tests! #,ntests)))
        (((Chapter e1* ...) e2* ...)
          (go #'(e2* ...) #'(chapter e1* ...) #'() #'() ntests (flush)))
        (((Section e1* ...) e2* ...)
          (go #'(e2* ...) #'(section e1* ...) #'() #'() ntests (flush)))
        (((Subsection e1* ...) e2* ...)
          (go #'(e2* ...) #'(subsection e1* ...) #'() #'() ntests (flush)))
        (((Exercise e1* ...) e2* ...)
          (go #'(e2* ...) #'(exercise e1* ...) #'() #'() ntests (flush)))
        ((e1 => e2 e* ...)
         (go=> #f #'(e2 e* ...) #'(e1 e2) header exports body (+ ntests 1) out))
        ((e1 ~> e2 e* ...)
         (go~> #f #'(e2 e* ...) #'(e1 e2) header exports body (+ ntests 1) out))
        ((e1 slow=> e2 e* ...)
         (go=> #t #'(e2 e* ...) #'(e1 e2) header exports body (+ ntests 1) out))
        ((e1 slow~> e2 e* ...)
         (go~> #t #'(e2 e* ...) #'(e1 e2) header exports body (+ ntests 1) out))
        (((define (name . args) e1* ...) e2* ...)
          (identifier? #'name)
          (with-syntax ((set #'(set! name (lambda args e1* ...))))
            (go #'(e2* ...)
                header (add-export #'name) #`(#,@body set) ntests out)))
        (((define name e1* ...) e2* ...)
          (identifier? #'name)
          (with-syntax ((set #'(set! name e1* ...)))
            (go #'(e2* ...)
                header (add-export #'name) #`(#,@body set) ntests out)))
        ((e e* ...)
          (go #'(e* ...) header exports #`(#,@body e) ntests out))))

    ;; Helper recursive function when parsing `=>` operators.
    (define (go=> slow x terms header exports body ntests out)
      (syntax-case x (=>)
        ((e2 => e3 e* ...)
          (go=> slow #'(e3 e* ...)
                #`(#,@terms e3) header exports body (+ ntests 1) out))
        ((e2 e* ...)
          (let ((new-body (add-assertion slow #'assert-equal terms body)))
            (go #'(e* ...) header exports new-body ntests out)))))

    ;; Helper recursive function when parsing `~>` operators.
    (define (go~> slow x terms header exports body ntests out)
      (syntax-case x (~>)
        ((e2 ~> e3 e* ...)
          (go=> slow #'(e3 e* ...)
                #`(#,@terms e3) header exports body (+ ntests 1) out))
        ((e2 e* ...)
          (let ((new-body (add-assertion slow #'assert-close terms body)))
            (go #'(e* ...) header exports new-body ntests out)))))

    ;; Helper used by `go=>` and `go~>` to add an assertion to `body`.
    (define (add-assertion slow assert terms body)
      (let* ((code #`(#,assert (list #,@terms) #'#,terms))
             (wrapped (if slow #`(when-slow #,code) code)))
        #`(#,@body #,wrapped)))

    (with-syntax (((_ e* ...) x))
      ;; PRINT INPUT
      ; #`(write '(e* ...))

      ;; PRINT MACRO OUTPUT
      ; #`(write '(begin #,@(go #'(e* ...) 'no-header #'() #'() #'())))

      ;; PRINT *entries*
      ; #`(begin #,@(go #'(e* ...) 'no-header #'() #'() #'()) (write *entries*))

      ;; NORMAL
      #`(begin #,@(go #'(e* ...) 'no-header #'() #'() 0 #'()))
      ))))