# SICP Study

This repository is my study of [_Structure and Interpretation of Computer Programs_][sicp] and its [lectures][]. The code is written in [R6RS Scheme][], tested with [Chez Scheme][], [Guile][], and [Racket][].

For more information, see [the website][].

## Exercises

### Usage

Use `./run.sh chez`, `./run.sh guile`, or `./run.sh racket` depending on your Scheme implementation.

To see the options, pass `--help`. For example, `./run.sh chez --help`.

Racket produces artifacts in `compiled/` directories. To remove them, run `make clean`.

### Structure

The program starts in [main.ss](main.ss). Each chapter of the book has its own file in [src/sicp/](src/sicp), written in a [domain-specific language](#language) implemented in [src/lang/core.ss](src/lang/core.ss). Source files in [src/compat](src/compat) reconcile differences between the supported Scheme implementations.

### Language

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
(Chapter :1 "chapter title") ; The ":" sigil is for the text

(define a 1) ; This belongs to Chapter 1

(Section :1.1 "section title")

(define b 2) ; This belongs to Section 1.1

(Section :1.1.1 "subsection title")

(define c 3) ; This belongs to Subsection 1.1.1

(Exercise ?1.1) ; The "?" sigil is for exercises

(define d 4) ; This belongs to Exercise 1.1
```

We can import definitions out of order:

```scheme
(Section :1.1 "first section"
  (use (:1.2 square)))   ; import square from Section 1.2

(define nine (square 3)) ; ok
(define eight (cube 2))  ; ERROR, 'cube' not defined

(Section :1.2 "second section")

(define (square x) (* x x))
(define (cube x) (* x x x))

(Exercise ?1.15
  (use :1.2 square cube))

(define a (+ (square 3) (cube 4))) ; ok
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

For more details, see [A Note on the Language][note].

## Notes

Study notes are stored in [notes/text.md](notes/text.md) and [notes/lecture.md](notes/lecture.md). They are written [Pandoc Markdown][], with some extras implemented by the [website generator](#website):

- Heading labels: `# 1A: Foo`, `## 1.2: Bar`.
- LaTeX math: inline `$...$`, display `$$...$$`.
- Internal links: `[](@1.2.3)` (Section 1.2.3 of the textbook), `[previous lecture](@2b)` (Lecture 2B), `[](:1.2)` (Section 1.2 in the exercises), `[](?1.23)` (Exercise 1.23).
- Citations: `[@1.2.3]` (Section 1.2.3 of the textbook), `[@1.2.fn42]` (footnote 42 on the Section 1.2 page), `[@1a.p3]` (page 3 of the transcript for Lecture 1A).
- Code blocks implicitly receive the `scheme` language class.
- Quotes wrapped with `::: highlight`/`:::` go on the Highlights page.
- Ranges like `1.1-6` wrapped with `::: exercises`/`:::` produce a list of exercise links.

## Website

In addition to the notes and exercises, this repository contains a custom static site generator. Its output is stored in [docs/](docs) and served on <https://mk12.github.io/sicp> by GitHub Pages.

The website is pure HTML5 and CSS. It contains no JavaScript.

### Usage

To build the website, make sure you have all the [dependencies](#dependencies) and then run `make docs`. A separate process builds each HTML file, so you can significantly speed this up by parallelizing, for example `make docs -j10`.

To view the website, open [docs/index.html](docs/index.html) in your browser.

### Implementation

The generator starts in [docgen.c](docgen.c). It semi-parses Markdown and Scheme, and renders things like navigation links, headings, and tables of contents. It then forks to Pandoc, which runs [filter.lua](notes/pandoc/filter.lua). The Lua filter deals with internal links, citations, code blocks, and math.

The math is the most complicated, because [KaTeX][] is implemented in JavaScript, not Lua. Instead of invoking it directly, the Lua filter communicates over a Unix pipe with [katex.ts](notes/pandoc/katex.ts), a [Deno][] server that uses the KaTeX module. The filter uses [luaposix][] to do this, which loads its C modules from `.so` files. We therefore require Pandoc to be dynamically linked to libc; the [fully static builds][static] provided for Linux will not work.

Pandoc highlights code with [skylighting][], which uses [Kate's XML syntax format][kate] to recognize languages. In this case it uses [scheme.xml](notes/pandoc/scheme.xml), which I modified from [the original][scheme.xml].

Pandoc assembles the result using [template.html](notes/pandoc/template.html). The template embeds SVGs from [notes/assets](notes/assets) rather than linking to them. (For SVGs that occur multiple times, it embeds them once at the top and then instantiates them with the `<use>` tag.)

Finally, `docgen`, post-processes the HTML and writes it in [docs/](docs).

The pages are styled by [style.css](docs/style.css). It follows the [BEM naming guide][bem].

## Contributing

Before submitting a PR, run `make`. This makes the following targets:

- `make lint`: Lints Scheme, TypeScript, and Bash.
- `make fmt`: Formats C and TypeScript.
- `make spell`: Spellchecks Markdown and Scheme.
- `make docs`: Builds the website.
- `make validate`: Validates HTML.
- `make test`: Tests with all Scheme implementations.

## Style

This project follows <http://community.schemewiki.org/?scheme-style>, with some changes:

- Disregard [Rule 3: Closing parens close the line](http://community.schemewiki.org/?scheme-style#H-vtpinr).
- Limit all lines to 80 columns.
- Insert one blank line between top-level definitions.
- Use alignment spaces only at the beginning of lines, never within them.
- Use `;;` for normal comments, and `;` for commented code/diagrams.
- Use `;` for inline comments. Separate from code by one space (or more for alignment).

Run `make lintss` to verify these rules.

## Editor support

Run `make vscode` to set up tasks for VS Code:

- `test`: Run all tests using Chez Scheme.
- `lint`: Lint all Scheme and Markdown files.

The `test` and `lint` tasks parse results into the Problems view, which you can advance through with <kbd>F8</kbd>.

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

`./run.sh guile` takes care of passing `--no-auto-compile` to ensure the interpreter is used.

## Dependencies

Run `./deps.sh check` to see if you're missing any dependencies, and (macOS only) `./deps.sh install` to install them.

- [Chez Scheme][], [Guile][], and [Racket][]: Scheme implementations.
- [Pandoc][]: Used to build the website. Must be dynamically linked to libc.
- [Lua][] and [luaposix][]: Used in the Pandoc filter.
- [Deno][]: Used to run a server that pre-renders math with [KaTeX][].
- [vnu][]: Used to validate HTML files.
- [clang-format][]: Used to format C files.

You also need a C compiler to compile [linter.c](linter.c) and [docgen.c](docgen.c).

## License

Most of the source code in this repository is available under the MIT License.

Some files in [src/sicp/](src/sicp), [notes/](notes), and [docs/](docs) are derivative works and are available under the CC BY-SA 4.0 License instead.

See [LICENSE](LICENSE.md) for details.

[sicp]: https://mitpress.mit.edu/sites/default/files/sicp/index.html
[the website]: https://mk12.github.io/sicp
[note]: https://mk12.github.io/sicp/exercise/language.html
[lectures]: https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/
[R6RS Scheme]: http://www.r6rs.org
[Chez Scheme]: https://cisco.github.io/ChezScheme/
[Guile]: https://www.gnu.org/software/guile/
[Racket]: http://racket-lang.org
[Pandoc]: https://pandoc.org
[Pandoc Markdown]: https://pandoc.org/MANUAL.html#pandocs-markdown
[Lua]: https://www.lua.org
[luaposix]: https://github.com/luaposix/luaposix
[static]: https://github.com/jgm/pandoc/issues/3986
[Deno]: https://deno.land
[KaTeX]: https://katex.org
[vnu]: https://validator.github.io/validator/
[clang-format]: https://clang.llvm.org/docs/ClangFormat.html
[skylighting]: https://github.com/jgm/skylighting
[kate]: https://docs.kde.org/trunk5/en/applications/katepart/highlight.html
[scheme.xml]: https://github.com/KDE/syntax-highlighting/blob/70b56cf8b3d1a85e15d1e09aa8490e5183967de0/data/syntax/scheme.xml
[bem]: http://getbem.com/naming/
