# SICP Study

This repository is my study of [_Structure and Interpretation of Computer Programs_][sicp]. It contains study notes and solutions to all the exercises. Solutions are written in portable [R6RS][] Scheme, tested with [Chez Scheme][], [Guile][], and [Racket][].

## Notes

My notes on the text are in [notes/notes.md](notes/notes.md). Run `make` in [notes/](notes) to compile them to HTML. You will need [Pandoc][] installed.

You can also continuously build the notes with `make watch`, which requires [entr][]. This is useful in combination with the VS Code extension [Live Server][].

## Exercises

### Usage

Use `./run.sh chez`, `./run.sh guile`, or `./run.sh racket` depending on which Scheme implementation you have installed.

To see the options, pass `--help`. For example, `./run.sh chez --help`.

### Structure

The program starts in `main.scm`. Each chapter of the book has its own file in [src/](src), written in a [custom DSL](#dsl). The DSL is implemented in [src/lang/core.ss](src/lang/core.ss). Source files in [src/compat](src/compat) reconcile differences between the supported Scheme implementations.

For exercises requiring proofs, I have written ConTeXt documents in [proofs/](proofs).

For exercises requiring diagrams, I have used ASCII art in comments or stored whiteboard photos in [whiteboard/](whiteboard).

### DSL

Tests use `=>`, `~>`, `=$>`, and `=!>`:

```scheme
(+ 1 2 3) => (+ 3 2 1) => 6            ; => asserts equality

(+ 1.0 0.1) ~> (- 1.2 0.1) ~> 1.1      ; ~> allows a small margin of error

(display "hi") =$> "hi"                ; =$> "..." tests standard output
(display "hi\nbye\n") =$> ["hi" "bye"] ; =$> [...] splits lines

(error 'foo "bad" 3) =!> "foo: bad: 3" ; =!> tests the error message
(error 'foo "bad" 3) =!> "bad"         ; any substring will do
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

SICP Study is available under the MIT License; see [LICENSE](LICENSE.md) for details.

[sicp]: https://mitpress.mit.edu/sites/default/files/sicp/index.html
[R6RS]: http://www.r6rs.org
[Chez Scheme]: https://cisco.github.io/ChezScheme/
[Guile]: https://www.gnu.org/software/guile/
[Racket]: http://racket-lang.org
[Pandoc]: https://pandoc.org
[entr]: http://eradman.com/entrproject/
[Live Server]: https://marketplace.visualstudio.com/items?itemName=ritwickdey.LiveServer
