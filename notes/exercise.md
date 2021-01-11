These are my solutions to the exercises in <cite>[Structure and Interpretation of Computer Programs][sicp]</cite>.

- webpages generate from code
- custom langauge built on R6RS
- works in C/G/R (compatibility shim)
- also textbook code
- modules explicit dependencies

[sicp]: https://mitpress.mit.edu/sites/default/files/sicp/index.html
[Chez]: https://cisco.github.io/ChezScheme/ "Chez Scheme"
[Guile]: https://www.gnu.org/software/guile/ "GNU Guile"
[Racket]: http://racket-lang.org "Racket programming language"
[r6rs]: http://www.r6rs.org
    "The Revised(6) Report on the Algorithmic Language Scheme"

# Note on the Language

Tests use `=>`, `~>`, `=$>`, and `=!>`:

```
(+ 1 2 3) => (+ 3 2 1) => 6            ; => asserts equality

(+ 1.0 0.1) ~> (- 1.2 0.1) ~> 1.1      ; ~> allows a small margin of error

(display "hi") =$> "hi"                ; =$> "..." tests standard output
(display "hi\nbye\n") =$> ["hi" "bye"] ; =$> [...] splits lines

(error 'foo "bad" 3) =!> "foo: bad: 3" ; =!> tests the error message
(error 'foo "bad" 3) =!> "bad"         ; any substring will do
```

Code fragments are isolated to their part of the book:

```
(Chapter :1 "chapter title")  ; The ":" sigil is for the text

(define a 1)  ; This belongs to Chapter 1

(Section :1.1 "section title")

(define b 2)  ; This belongs to Section 1.1

(Section :1.1.1 "subsection title")

(define c 3)  ; This belongs to Subsection 1.1.1

(Exercise ?1.1)  ; The "?" sigil is for exercises

(define d 4)  ; This belongs to Exercise 1.1
```

We can import definitions out of order:

```
(Section :1.1 "first section"
  (use (:1.2 square)))    ; import square from Section 1.2

(define nine (square 3))  ; ok
(define eight (cube 2))   ; ERROR, 'cube' not defined

(Section :1.2 "second section")

(define (square x) (* x x))
(define (cube x) (* x x x))

(Exercise ?1.15
  (use :1.2 square cube))

(define a (+ (square 3) (cube 4)))  ; ok
```

We can also unhygienically paste code, but only from earlier sections in the same file:

```
(Section :1 "Original")

(define a 1)
(define (inc x) (+ x a))
(inc 42) => 43

(Section :2 "Copy")

(define a -1)
(paste (:1 inc))
(inc 42) => 41
```
