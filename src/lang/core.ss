;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src lang core)
  (export SICP Chapter Section Exercise define => ~> =?> =$> =!> =>... paste
          capture-output hide-output
          run-sicp)
  (import (except (rnrs (6)) current-output-port define-syntax)
          (rnrs mutable-pairs (6))
          (rnrs mutable-strings (6))
          (rename (src compat) (extended-define-syntax define-syntax)))

;; Global flag for whether to use ANSI color in output.
(define *color* #f)

;; Returns `str` with ANSI escape codes to make it `color`, if `*color*` is set.
;; Otherwise, returns `str` unchanged.
(define (ansi color str)
  (if (not *color*)
      str
      (let ((code (case color
                    ((bold) "1")
                    ((bold-red) "1;31")
                    ((red) "31")
                    ((green) "32")
                    ((yellow) "33")
                    ((blue) "34")
                    ((magenta) "35")
                    (else (error 'ansi "unknown color" color)))))
        (string-append "\x1b;[" code "m" str "\x1b;[0m"))))

;; Global test counters, used for reporting and for setting the exit status.
;; When running without a filter, total = passes + fails. Otherwise, the
;; difference is equal to the number of tests filtered out.
(define *total* 0)
(define *passes* 0)
(define *fails* 0)

;; Increases `*total*` by `n` tests. We need a separate function for this
;; because using `*total*` in macros would indirectly export it, meaning it
;; would have to be immutable.
(define (increase-total-tests! n)
  (set! *total* (+ *total* n)))

;; Records that a test passed.
(define (test-pass!)
  (set! *passes* (+ *passes* 1)))

;; Records that a test failed and displays a failure message, including the
;; source location of syntax object `expr` and the string `msg`.
(define (test-fail! expr msg)
  (set! *fails* (+ *fails* 1))
  (let-values (((file line col) (syntax->location expr)))
    (display
     (format (string-append (ansi 'bold "~a:~a:~a: assertion failed") "\n~a")
             file line col msg))))

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

;; Asserts that each number in `vals` is very close to the previous one. Expects
;; `exprs` to contain syntax objects for each value in `vals`.
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
;; `(pred? v1 v2)` is false, the error message includes the `v1` line number and
;; string `(make-msg v1 v2 d1 d2)`, where `d1` and `d2` are the corresponding
;; unevaluated forms as data (not syntax objects). Also increments `*passes*` or
;; `*fails*` based on the result.
(define (assert-on-pairs pred? make-msg vals exprs)
  (for-each
   (lambda (vpair epair)
     (let ((v1 (car vpair))
           (v2 (cdr vpair))
           (e1 (car epair))
           (e2 (cdr epair)))
       (cond ((pred? v1 v2) (test-pass!))
             (else (test-fail!
                    e1
                    (make-msg v1 v2 (syntax->datum e1) (syntax->datum e2)))))))
   (pairs vals)
   (pairs (syntax->list exprs))))

;; Asserts that `val` is `equal?` to one of the items in `alts`. Expects
;; `val-expr` and `alts-expr` to contain their respective syntax objects. Note:
;; `alts-expr` does not evaluate to `alts`, but rather each item of `alts-exprs`
;; evaluates to the corresponding item of `alts`.
(define (assert-member val alts val-expr alts-expr)
  (define (fmt-alts-expr)
    (let ((s (format "~a" (syntax->datum alts-expr))))
      (string-set! s 0 #\[)
      (string-set! s (- (string-length s) 1) #\])
      s))
  (define (fmt-alts)
    (define (slots xs)
      (cond ((null? xs) '())
            ((null? (cdr xs)) (list (ansi 'green "~s")))
            (else (cons (string-append (ansi 'green "~s") " | ")
                        (slots (cdr xs))))))
    (apply format (apply string-append (slots alts)) alts))
  (if (member val alts)
      (test-pass!)
      (test-fail!
       val-expr
       (format
        (string-append
         "left: " (ansi 'blue "~s") "\n=> " (ansi 'green "~s") "\n\n"
         "right: " (ansi 'blue "~a") "\n=?> ~a\n\n")
        (syntax->datum val-expr) val (fmt-alts-expr) (fmt-alts)))))

;; Asserts that the string `output` matches `expected`, a syntax object
;; containing either a string or an (unquoted) list. In the string case, we test
;; for equality. In the list case, we split `output` using `split-lines` and
;; compare with the list of strings. Expects `expr` to be the syntax object for
;; the expression which produced the output.
(define (assert-output output expr expected)
  (define (fmt str-or-list)
    (define (slots xs)
      (cond ((null? xs) '())
            ((null? (cdr xs)) (list "~s"))
            (else (cons "~s\n     " (slots (cdr xs))))))
    (if (string? str-or-list)
        (format (ansi 'yellow "~s") str-or-list)
        (apply format
               ;; In R6RS square brackets are interchangeable with parens. I do
               ;; not use them in cond/let/etc. the way the spec does. I just
               ;; use them in =$> and =?> to make them stand out.
               (string-append
                "["
                (ansi 'yellow (apply string-append (slots str-or-list)))
                "]")
               str-or-list)))
  (let* ((expected (syntax->datum expected))
         (output (cond ((string? expected) output)
                       ((list? expected) (split-lines output))
                       (else (error 'assert-output
                                    "result must be a string or list"
                                    expected)))))
    (if (equal? output expected)
        (test-pass!)
        (test-fail!
         expr
         (format
          (string-append
           "left: " (ansi 'blue "~s") "\n=$> ~a\n\nright:\n=$> ~a\n\n")
          (syntax->datum expr) (fmt output) (fmt expected))))))

;; Asserts that executing `thunk` raises an error, and that the error message
;; formatted as "<who>: <message>: <irritants>" contains `substr` as a
;; substring. Expects `expr` to be the syntax object for the body of `thunk`.
(define (assert-raises thunk expr substr)
  (define (format-condition con)
    (define (join sep items)
      (cond ((null? items) "")
            ((null? (cdr items)) (format "~a" (car items)))
            (else (format "~a~a~a" (car items) sep (join sep (cdr items))))))
    (define-syntax coalesce
      (syntax-rules ()
        ((_ (test exp) ...) (append (if test (list exp) '()) ...))))
    (join ": "
          (coalesce ((who-condition? con) (condition-who con))
                    ((message-condition? con) (condition-message con))
                    ((and (irritants-condition? con) (condition-irritants con))
                     (join " " (condition-irritants con))))))
  (define (fail! raised? result)
    (test-fail!
     expr
     (format
      (string-append
       "left: " (ansi 'blue "~s") "\n" (if raised? "=!>" "=>") " "
       (ansi (if raised? 'red 'green) (if raised? "~a" "~s")) "\n\n"
       "right:\n=!> ... " (ansi 'red substr) " ...\n\n")
      (syntax->datum expr) result)))
  (call/cc
   (lambda (return)
     (let ((result
            (with-exception-handler
             (lambda (con)
               (let ((str (format-condition con)))
                 (if (string-contains? str substr)
                     (test-pass!)
                     (fail! #t str)))
               (return))
             thunk)))
       (fail! #f result)))))

;; Asserts that executing `thunk` does not terminate after a short time. Expects
;; `expr` to be the syntax object for the body of `thunk`.
(define (assert-nonterminating thunk expr)
  (let ((result (run-with-short-timeout thunk)))
    (if (null? result)
        (test-pass!)
        (test-fail!
         expr
         (format
          (string-append
           "left: " (ansi 'blue "~s") "\n=> ~s\n\nright:\n=>... "
           "(expected to never terminate)\n\n")
          (syntax->datum expr) (car result))))))

;; Captures standard output in a string.
(define-syntax capture-output
  (syntax-rules ()
    ((_ e* ...)
     (with-output-to-string (lambda () e* ...)))))

;; Supresses printing to standard output.
(define-syntax hide-output
  (syntax-rules ()
    ((_ e* ...)
     (parameterize ((current-output-port (open-output-string)))
       e* ...))))

;; Splits a string into a list of lines, using "\n" as the delimiter, not
;; including the delimiter in the result, and not producing empty strings for
;; runs of multiple newlines or for newlines at the start/end.
(define (split-lines s)
  (let ((len (string-length s)))
    (let loop ((i len) (j len) (res '()))
      (define (push)
        (if (< i j) (cons (substring s i j) res) res))
      (cond ((zero? i) (push))
            ((char=? (string-ref s (- i 1)) #\newline)
             (loop (- i 1) (- i 1) (push)))
            (else (loop (- i 1) j res))))))

;; Converts a syntax object to a list of syntax objects.
;; https://www.scheme.com/csug8/syntax.html#./syntax:s6
(define (syntax->list ls)
  (syntax-case ls ()
    (() '())
    ((x . r) (cons #'x (syntax->list #'r)))))

;; Returns a list of the adjacent pairs of elements in `xs` For example, given
;; the list `(a b c d)` it returns `((a . b) (b . c) (c . d))`.
(define (pairs xs)
  (cond ((null? xs) '())
        ((null? (cdr xs)) '())
        (else (cons (cons (car xs) (cadr xs))
                    (pairs (cdr xs))))))

;; Like hashtable-ref, but fails if the key is not present.
(define (hashtable-ref-must table key)
  (let ((val (hashtable-ref table key #f)))
    (if (and (eq? val #f) (not (hashtable-contains? table key)))
        (error 'hashtable-ref-must "key not found" key)
        val)))

;; An entry stores code from a part of the textbook. It consists of a unique
;; symbol `id`, a kind ('Chapter, 'Section, or 'Exercise), a string `num`
;; containing a dotted number like "1.2.3", a title string (or #f), a list of
;; imported names from other entries formatted as `((id name ...) ...)`, a list
;; of exported names, and a thunk taking all the imported names as one flat list
;; of arguments and returns the exported values in the same order as `exports`.
(define-record-type entry
  (fields id kind num title imports exports thunk))

;; A queue supports constant time appending to the back, popping from the front,
;; and accessing the length. (It also supports pushing to the front, making it
;; more of a deque, but I didn't feel like renaming everything).
(define-record-type (queue make-raw-queue queue?)
  (fields (mutable length) (mutable front) (mutable back)))
(define (make-queue)
  (make-raw-queue 0 '() '()))
(define (queue-empty? q)
  (null? (queue-front q)))
(define (queue-push-back! q x)
  (let ((new-pair (cons x '())))
    (queue-length-set! q (+ (queue-length q) 1))
    (cond ((queue-empty? q)
           (queue-front-set! q new-pair)
           (queue-back-set! q new-pair))
          (else
           (set-cdr! (queue-back q) new-pair)
           (queue-back-set! q new-pair)))))
(define (queue-pop-front! q)
  (if (queue-empty? q)
      (error 'queue-pop-front! "empty queue")
      (let ((result (car (queue-front q))))
        (cond ((eq? (queue-front q) (queue-back q))
               (queue-front-set! q '())
               (queue-back-set! q '()))
              (else
               (queue-front-set! q (cdr (queue-front q)))))
        result)))
(define (queue-push-front! q x)
  (if (queue-empty? q)
      (queue-push-back! q x)
      (queue-front-set! q (cons x (queue-front q)))))

;; Global queue of entries. The `SICP` macro produces calls to `add-entry!`.
(define *entries* (make-queue))
(define (add-entry! id kind num title imports exports thunk)
  (queue-push-back! *entries*
                    (make-entry id kind num title imports exports thunk)))

;; Converts `*entries*` to a hashtable from `id` to entries. Raises an error if
;; there are two entries with the same `id`.
(define (entries-by-id)
  (define by-id (make-eq-hashtable (queue-length *entries*)))
  (for-each
   (lambda (entry)
     (when (hashtable-contains? by-id (entry-id entry))
       (error 'entries-by-id "duplicate entry id" (entry-id entry)))
     (hashtable-set! by-id (entry-id entry) entry))
   (queue-front *entries*))
  by-id)

;; Returns a hashtable from entries in `*entries*` to their in-degrees (the
;; number of times they are imported). Includes only entries that satisfy
;; `pred?` and all their transitive dependencies. Edges from excluded entries do
;; not count towards in-degree counts for the included entres. Requires `by-id`,
;; a hashtable produced by `entries-by-id`.
(define (entries-to-in-degrees by-id pred?)
  (define in-degrees (make-eq-hashtable))
  (define q (make-queue))
  (for-each
   (lambda (entry)
     (when (pred? entry)
       (queue-push-back! q entry)))
   (queue-front *entries*))
  ;; Explore entries reachable from those satisfying `pred?` using BFS.
  (let loop ()
    (unless (queue-empty? q)
      (let* ((e (queue-pop-front! q))
             (deps (map (lambda (import-list)
                          (unless (hashtable-contains? by-id (car import-list))
                            (error 'entries-to-in-degrees
                                   (format "~a imports from nonexistent ~a"
                                           (entry-id e)
                                           (car import-list))))
                          (hashtable-ref-must by-id (car import-list)))
                        (entry-imports e))))
        (hashtable-set! in-degrees e 0)
        (for-each
         (lambda (d)
           (unless (hashtable-contains? in-degrees d)
             (queue-push-back! q d)))
         deps)
        (loop))))
  ;; Iterate over the entries and calculate in-degrees.
  (vector-for-each
   (lambda (e)
     (for-each
      (lambda (import-list)
        (let ((d (hashtable-ref-must by-id (car import-list))))
          (when (hashtable-contains? in-degrees d)
            (hashtable-update! in-degrees
                               d
                               (lambda (x) (+ x 1))
                               #f))))
      (entry-imports e)))
   (hashtable-keys in-degrees))
  in-degrees)

;; Topologically sorts entries from `*entries*`, including only those that
;; satisfy `pred?` and all their transitive dependencies. Requires `by-id`, a
;; hashtable produced by `entries-by-id`. Raises an error if sorting fails
;; because of an import cycle.
(define (sort-entries by-id pred?)
  (define in-degrees (entries-to-in-degrees by-id pred?))
  (define sources (make-queue))
  (define sorted '())
  (for-each
   (lambda (entry)
     (when (zero? (hashtable-ref in-degrees entry -1))
       (queue-push-back! sources entry)))
   ;; Go through all of `*entries*` in reverse, rather than just the entries
   ;; filtered by `pred?`, so that in the absence of forward dependencies
   ;; entries will be executed in source order.
   (reverse (queue-front *entries*)))
  (let loop ()
    (unless (queue-empty? sources)
      (let ((node (queue-pop-front! sources)))
        (set! sorted (cons node sorted))
        (for-each
         (lambda (import-list)
           (let* ((e (hashtable-ref-must by-id (car import-list)))
                  (deg (hashtable-ref-must in-degrees e))
                  (new-deg (- deg 1)))
             (hashtable-set! in-degrees e new-deg)
             (when (zero? new-deg)
               ;; Push to front so that we add entries as soon as they turn
               ;; into sources. The result is that dependencies are moved
               ;; to occur just before they are used, and no earlier.
               (queue-push-front! sources e))))
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

;; Executes the code in `*entries*`. If `verbose` is true, prints verbose info
;; about all tests. If `color` is true, prints color output. If `filters` is
;; nonempty, only runs entries whose `id` matches at least one of the filters
;; (and all their transitive depedencies). Returns #t if all tests passed.
(define (run-sicp filters verbose color)
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
      (let* ((exporter (hashtable-ref-must by-id (car import-list)))
             (import-names (cdr import-list))
             (export-names (entry-exports exporter))
             (export-values (hashtable-ref-must results exporter)))
        (define (find-value name)
          (let loop ((en export-names)
                     (ev export-values))
            (cond ((or (null? en) (null? ev))
                   (error 'run-sicp
                          (format "~a imports nonexistent `~a` from ~a"
                                  (entry-id importer)
                                  name
                                  (entry-id exporter))))
                  ((eq? name (car en)) (car ev))
                  (else (loop (cdr en) (cdr ev))))))
        (map find-value import-names)))
    (fold-left append '() (map gather (entry-imports importer))))
  (when color (set! *color* #t))
  (for-each
   (lambda (e)
     (when verbose
       (display
        (ansi 'yellow
              (format "* ~a ~a~a\n"
                      (symbol->string (entry-kind e))
                      (entry-num e)
                      (if (entry-title e)
                          (string-append ": " (entry-title e))
                          "")))))
     (hashtable-set! results
                     e
                     (apply (entry-thunk e) (gather-args e))))
   sorted)
  (when verbose (newline))
  (display
   (format "test result: ~a. ~a passed; ~a failed; ~a filtered out\n"
           (if (zero? *fails*) (ansi 'green "ok") (ansi 'bold-red "FAIL"))
           *passes* *fails* (- *total* *passes* *fails*)))
  (when (and (zero? *passes*) (zero? *fails*))
    (display (ansi 'magenta "WARNING: did not run any tests\n")))
  (zero? *fails*))

;; In order for `SICP` to match on auxiliary keywords, we must export them. That
;; implies giving them definitions. We don't need to do this for `define` and
;; `=>` because we instead re-export the rnrs definitions (the latter is already
;; an auxiliary keyword used in `cond`).
(let-syntax
  ((auxiliary
    (syntax-rules ()
      ((_ lit ...)
       (begin (define-syntax (lit x)
                (syntax-violation #f "incorrect usage of auxiliary keyword" x))
              ...)))))
  (auxiliary Chapter Section Exercise ~> =?> =$> =!> =>... paste))

;; A DSL for SICP code samples and exercises. `(SICP reg e* ...)` defines a
;; function named `reg` that registers all the definitions produced by the
;; expressions that follow, which are written in the DSL.
(define-syntax (SICP x)
  ;; Creates an entry `num` from its `id` syntax. This just chops off the sigil
  ;; used to differentiate chapters/sections from exercises (and also used
  ;; because `1.1.1` on its own is an invalid R6RS identifier).
  (define (entry-id->num id)
    (let ((str (symbol->string (syntax->datum id))))
      (substring str 1 (string-length str))))

  ;; Table of definitions used to implement `paste`.
  (define definitions (make-eq-hashtable))

  ;; Construct a combined identifier from an entry id and a name. This is used
  ;; as a key in `definitions`.
  (define (paste-id id name)
    (define (str s) (symbol->string (syntax->datum s)))
    (string->symbol (string-append (str id) "--" (str name))))

  ;; Given a spec like `((:1.2 foo) (?3.4 bar baz))`, retrieve the code for
  ;; `foo`, `bar`, and `baz`. The returned syntax should be spliced, since
  ;; otherwise it would be invoking void as a procedure.
  (define (retrieve-paste-code spec)
    (define (inner id names)
      (syntax-case names ()
        (() #'())
        ((name e* ...)
         (let ((pid (paste-id id #'name)))
           (unless (hashtable-contains? definitions pid)
             (syntax-violation #f "paste of non-existent definition" #'name))
           #`(#,(hashtable-ref definitions pid #f)
              #,@(inner id #'(e* ...)))))))
    (syntax-case spec ()
      (() #'())
      (((id name* ...) e* ...)
       #`(#,@(inner #'id #'(name* ...))
          #,@(retrieve-paste-code #'(e* ...))))))

  ;; Recursive implementation of the macro. It takes:
  ;; x       - the remaining forms to be processed
  ;; header  - the last seen chapter/section/exercise header
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
        ;; Make sure no imports are shadowed by definitions.
        (let ((all-import-names (syntax->datum #'(import-name ... ...))))
          (for-each
           (lambda (name)
             (when (memq name all-import-names)
               (syntax-violation
                #f
                (string-append "imported name '"
                               (symbol->string name)
                               "' is shadowed by a local definition")
                header)))
           (syntax->datum exports)))
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
    (define (add-export name code)
      (with-syntax (((_ id e* ...) header))
        (let ((pid (paste-id #'id name)))
          (cond ((memq (syntax->datum name) (syntax->datum exports))
                 ;; This is a redefinition of the same export. Remove it from
                 ;; the definition table to disallow pasting the code, since it
                 ;; would be ambiguous which definition it is meant to paste.
                 (hashtable-delete! definitions pid)
                 exports)
                (else
                 (when code (hashtable-set! definitions pid code))
                 #`(#,@exports #,name))))))
    (define (add-exports-from-paste names)
      (define (add names result)
        (syntax-case names ()
          (() result)
          ((n n* ...)
           (memq (syntax->datum #'n) (syntax->datum exports))
           (add #'(n* ...) result))
          ((n n* ...)
           (add #'(n* ...) #`(#,@result n)))))
      ;; This intentionally doesn't add to the definitions table. Pastes should
      ;; come from the original source -- it would be too confusing if a paste's
      ;; code comes from another paste.
      (add names exports))
    (syntax-case x (Chapter Section Exercise define
                    => ~> =?> =$> =!> =>... paste) ; NOALIGN
      (() #`(#,@(flush) (increase-total-tests! #,ntests)))
      (((Chapter e1* ...) e2* ...)
       (go #'(e2* ...) (car x) #'() #'() ntests (flush)))
      (((Section e1* ...) e2* ...)
       (go #'(e2* ...) (car x) #'() #'() ntests (flush)))
      (((Exercise e1* ...) e2* ...)
       (go #'(e2* ...) (car x) #'() #'() ntests (flush)))
      (((define name) e* ...)
       (identifier? #'name)
       (go #'(e* ...) header (add-export #'name #f) body ntests out))
      (((define name e1* ...) e2* ...)
       (identifier? #'name)
       (with-syntax ((set #'(set! name e1* ...)))
         (go #'(e2* ...)
             header (add-export #'name #'set) #`(#,@body set) ntests out)))
      (((define (name . args) e1* ...) e2* ...)
       (identifier? #'name)
       (with-syntax ((set #'(set! name (lambda args e1* ...))))
         (go #'(e2* ...)
             header (add-export #'name #'set) #`(#,@body set) ntests out)))
      ((e1 => e2 e* ...)
       (go=> #'(e* ...) #'(e1 e2) header exports body (+ ntests 1) out))
      ((e1 ~> e2 e* ...)
       (go~> #'(e* ...) #'(e1 e2) header exports body (+ ntests 1) out))
      ((e1 =?> e2 e* ...)
       (list? (syntax->datum #'e2))
       (with-syntax ((assert #`(assert-member e1 (list #,@#'e2) #'e1 #'e2)))
         (go #'(e* ...) header exports #`(#,@body assert) (+ ntests 1) out)))
      ((e1 =$> e2 e* ...)
       (with-syntax ((assert #'(assert-output (capture-output e1) #'e1 #'e2)))
         (go #'(e* ...) header exports #`(#,@body assert) (+ ntests 1) out)))
      ((e1 =!> e2 e* ...)
       (string? (syntax->datum #'e2))
       (with-syntax ((assert #'(assert-raises (lambda () e1) #'e1 e2)))
         (go #'(e* ...) header exports #`(#,@body assert) (+ ntests 1) out)))
      ((e =>... e* ...)
       (with-syntax ((assert #'(assert-nonterminating (lambda () e) #'e)))
         (go #'(e* ...) header exports #`(#,@body assert) (+ ntests 1) out)))
      (((paste (id name ...) ...) e* ...)
       (with-syntax (((code ...) (retrieve-paste-code #'((id name ...) ...))))
         (go #'(e* ...)
             header (add-exports-from-paste #'(name ... ...))
             #`(#,@body code ...) ntests out)))
      ((e e* ...)
       (go #'(e* ...) header exports #`(#,@body e) ntests out))))

  ;; Helper recursive function when parsing `=>` operators.
  (define (go=> x terms header exports body ntests out)
    (syntax-case x (=>)
      ((=> e e* ...)
       (go=> #'(e* ...) #`(#,@terms e) header exports body (+ ntests 1) out))
      ((e* ...)
       (with-syntax ((assert #`(assert-equal (list #,@terms) #'#,terms)))
         (go #'(e* ...) header exports #`(#,@body assert) ntests out)))))

  ;; Helper recursive function when parsing `~>` operators.
  (define (go~> x terms header exports body ntests out)
    (syntax-case x (~>)
      ((~> e e* ...)
       (go~> #'(e* ...) #`(#,@terms e) header exports body (+ ntests 1) out))
      ((e* ...)
       (with-syntax ((assert #`(assert-close (list #,@terms) #'#,terms)))
         (go #'(e* ...) header exports #`(#,@body assert) ntests out)))))

  (with-syntax (((_ register e* ...) x))
    #`(define (register) #,@(go #'(e* ...) 'no-header #'() #'() 0 #'()))))

) ; end of library
