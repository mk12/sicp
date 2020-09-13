# SICP Exercises

This repository contains my solutions to exercises from [_Structure and Interpretation of Computer Programs_][sicp]. It is written in portable [R6RS][] Scheme, tested with [Chez Scheme][], [Guile][], and [Racket][].

## Usage

Use `./run.sh chez`, `./run.sh guile`, or `./run.sh racket` depending on which Scheme implementation you have installed.

To see the options, pass `--help`. For example, `./run.sh chez --help`.

## Structure

The program starts in `main.scm`. Each chapter of the book has its own file in [src/](src), written in a [custom DSL](#dsl). The DSL is implemented in [src/lang/core.ss](src/lang/core.ss). Source files in [src/compat](src/compat) reconcile differences between the supported Scheme implementations.

For exercises requiring proofs, I have written ConTeXt documents in [proofs/](proofs).

For exercises requiring diagrams, I have used ASCII art in comments or stored whiteboard photos in [whiteboard/](whiteboard).

## DSL

Here is a quick guide to the DSL used in this project.

Tests are written using `=>` and `~>`:

```scheme
(+ 1 1) => 2        ; This asserts that (+ 1 1) evaluates to 2
1 => 1 => 1         ; The operator can be chained
(+ 1.0 0.1) ~> 1.1  ; Use ~> for inexact numbers (allows a small margin of error)
```

You can test standard output with `=/>`:

```scheme
(display "hi") =/> "hi"
(display "the quick brown fox\njumps over the lazy dog\n")
=/> ["the quick brown fox"
     "jumps over the lazy dog"]
```

And test expected errors with `=!>`:

```scheme
(error 'foo "unexpected" 3) =!> "foo: unexpected: 3" ; Test the error message
(error 'foo "unexpected" 3) =!> "unexpected"         ; Any substring will do
```

Code fragments are isolated to their part of the book:

```scheme
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

```scheme
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

```scheme
(Section :1 "Original")

(define a 1)
(define (inc x) (+ x a))
(inc 42) => 43

(Section :2 "Copy")

(define a -1)
(paste (:1 inc))
(inc 42) => 41
```

## Known issues

### Line numbers

Test failures do not show line numbers in Guile.

### Mutating quoted lists

The Scheme implementation must support mutating quoted lists:

```scheme
(define x '(a b))
(set-car! x 'c)

(write x)      ; (c b)
(write '(a b)) ; (a b)
```

This works in Chez Scheme and Racket. It also works in Guile, but only the interpreter, not the compiler. When compiled with Guile, the line `(write '(a b))` above produces the bizarre result `(c b)`, because the `set-car!` call modified the interned representation of `'(a b)`.

`run.sh guile` takes care of passing `--no-auto-compile` to ensure the interpreter is used.

## License

© 2020 Mitchell Kember

SICP Exercises is available under the MIT License; see [LICENSE](LICENSE.md) for details.

[sicp]: https://mitpress.mit.edu/sites/default/files/sicp/index.html
[R6RS]: http://www.r6rs.org
[Chez Scheme]: https://cisco.github.io/ChezScheme/
[Guile]: https://www.gnu.org/software/guile/
[Racket]: http://racket-lang.org
