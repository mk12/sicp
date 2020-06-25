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

Slow tests only run with `-s` or `--slow`:

```scheme
(fib 100)
slow=> 354224848179261915075
```

Code fragments are isolated to their part of the book:

```scheme
(Chapter :1 "chapter title")    ; The ":" sigil is for the text

(define a 1)  ; This belongs to Chapter 1

(Section :1.1 "section title")  ; The "?" sigil is for exercises

(define b 2)  ; This belongs to Section 1.1

(Subsection :1.1.1 "subsection title")

(define c 3)  ; This belongs to Subsection 1.1.1

(Exercise ?1.1)

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

## Known issues

- Test failures do not show line numbers in Guile.

## License

© 2020 Mitchell Kember

SICP Exercises is available under the MIT License; see [LICENSE](LICENSE.md) for details.

[sicp]: https://mitpress.mit.edu/sites/default/files/sicp/index.html
[R6RS]: http://www.r6rs.org
[Chez Scheme]: https://cisco.github.io/ChezScheme/
[Guile]: https://www.gnu.org/software/guile/
[Racket]: http://racket-lang.org
