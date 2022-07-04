# SICP Study

This repository is my study of [_Structure and Interpretation of Computer Programs_][sicp] and its [lectures]. The code is written in [R6RS Scheme], tested with [Chez Scheme], [Guile], and [Racket].

For more information, see [the website].

## Exercises

### Usage

Use `./run.sh chez`, `./run.sh guile`, or `./run.sh racket` depending on your Scheme implementation. Use `./run.sh chezd` to start the Chez debugger when an error occurs.

To see the options, pass `--help`. For example, `./run.sh chez --help`.

Racket produces artifacts in `compiled/` directories. To remove them, run `make clean`.

### Structure

The program starts in [main.ss]. Each chapter of the book has its own file in [src/sicp/], written in a [domain-specific language](#language) implemented in [src/lang/core.ss]. Source files in [src/compat/] reconcile differences between the supported Scheme implementations.

### Language

Tests use `=>`, `~>`, `=?>`, `=$>`, `=!>`, and `=>...`:

```scheme
(+ 1 2 3) => (+ 3 2 1) => 6            ; => asserts equality

(+ 1.0 0.1) ~> (- 1.2 0.1) ~> 1.1      ; ~> allows a small margin of error

(random 5) =?> [0 1 (+ 1 1) 3 4]       ; =?> is for nondeterministic tests

(display "hi") =$> "hi"                ; =$> "..." tests standard output
(display "hi\nbye\n") =$> ["hi" "bye"] ; =$> [...] splits lines

(error 'foo "bad" 3) =!> "foo: bad: 3" ; =!> tests the error message
(error 'foo "bad" 3) =!> "bad"         ; any substring will do

(let loop () (loop)) =>...             ; =>... asserts nontermination
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
(Section :1.1 "First section"
  (use (:1.2 square)))   ; import square from Section 1.2

(define nine (square 3)) ; ok
(define eight (cube 2))  ; ERROR, 'cube' not defined

(Section :1.2 "Second section")

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

Study notes are stored in [notes/text.md] and [notes/lecture.md]. They are written [Pandoc Markdown], with some extras implemented by the [website generator](#website):

- Heading labels: `# 1A: Foo`, `## 1.2: Bar`.
- LaTeX math: inline `$...$`, display `$$...$$`.
- Internal links: `[](@1.2.3)` (Section 1.2.3 of the textbook), `[previous lecture](@2b)` (Lecture 2B), `[](:1.2)` (Section 1.2 in the exercises), `[](?1.23)` (Exercise 1.23).
- Citations: `[@1.2.3]` (Section 1.2.3 of the textbook), `[@1.2.fn42]` (footnote 42 on the Section 1.2 page), `[@1a.p3]` (page 3 of the transcript for Lecture 1A).
- Code blocks implicitly receive the `scheme` language class.
- Quotes wrapped with `::: highlight`/`:::` go on the Highlights page.
- Ranges like `1.1-6` wrapped with `::: exercises`/`:::` produce a list of exercise links.

## Website

In addition to the notes and exercises, this repository contains a custom static site generator. Its output is stored in [docs/] and served on <https://mk12.github.io/sicp> by GitHub Pages.

The website is pure HTML5 and CSS. It contains no JavaScript.

### Usage

To build the website, make sure you have all the [dependencies](#dependencies) and then run `make docs`. A separate process builds each HTML file, so you can significantly speed this up by parallelizing, for example `make docs -j10`.

To view the website, open [docs/index.html] in your browser.

Use `./watch.sh` to live-reload the website while you edit sources.

### Implementation

The generator starts in [docgen.c]. It semi-parses Markdown and Scheme, and renders things like navigation links, headings, and tables of contents. It then forks to Pandoc, which runs [filter.lua]. The Lua filter deals with internal links, citations, code blocks, math, and diagrams.

To render math and diagrams, the Lua filter communicates over a Unix socket with the render.ts server (described below). It uses [luaposix] to do this, which loads its C modules from `.so` files. We therefore require Pandoc to be dynamically linked to libc; the [fully static builds][static] provided for Linux will not work.

The render.ts server is a [Deno] server implemented in [render.ts]. It serves requests in a simple text-based protocol over a Unix socket. It renders math using [KaTeX], and converts ASCII diagrams to SVG using [svgbob] and [svgo]. The benefit of this approach, rather than invoking these tools directly in the Lua filter, is that it avoids spawning a new process for every piece of inline math.

Pandoc highlights code with [skylighting], which uses [Kate's XML syntax format][kate] to recognize languages. In this case it uses [scheme.xml], which I modified from [the original][kde-scheme.xml].

Pandoc assembles the result using [template.html]. The template embeds SVGs from [notes/assets/] rather than linking to them. (For SVGs that occur multiple times, it embeds them once at the top and then instantiates them with the `<use>` tag.)

Finally, `docgen` post-processes the HTML and writes it in [docs/].

The pages are styled by [style.css]. It follows the [BEM naming guide][bem].

## Contributing

Before submitting a PR, run `make`. This makes the following targets:

- `make lint`: Lints Scheme, TypeScript, and Bash.
- `make fmt`: Formats C, Objective-C, and TypeScript.
<!-- - `make spell`: Spellchecks Markdown and Scheme (macOS only). -->
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

Run `make clangd` to create compile_commands.json for C/C++ LSP support.

Run `make vscode` to set up tasks for VS Code:

- `test`: Run all tests using Chez Scheme.
- `lint`: Lint all Scheme and Markdown files.

The `test` and `lint` tasks parse results into the Problems view, which you can advance through with <kbd>F8</kbd>.

## Dependencies

Run `./deps.sh check` to see if you're missing any dependencies, and (macOS only) `./deps.sh install` to install them.

- [Chez Scheme], [Guile], and [Racket]: Scheme implementations.
- [Pandoc]: Used to build the website. Must be dynamically linked to libc.
- [LuaRocks] and [luaposix]: Used in the Pandoc filter.
- [Deno]: Used to run a server that renders math and diagrams.
- [svgbob]: Used to convert ASCII diagrams to SVG.
- [vnu]: Used to validate HTML files.
- [clang-format]: Used to format C files.

You also need a C compiler to compile [lint.c] and [docgen.c].

## License

Most of the source code in this repository is available under the MIT License.

Some files in [src/sicp/], [notes/], and [docs/] are derivative works and are available under the CC BY-SA 4.0 License instead.

See [LICENSE](LICENSE.md) for details.

[docgen.c]: tools/docgen.c
[docs/]: docs/
[docs/index.html]: docs/index.html
[filter.lua]: notes/pandoc/filter.lua
[lint.c]: tools/lint.c
[main.ss]: src/main.ss
[notes/]: notes/
[notes/assets/]: notes/assets/
[notes/lecture.md]: notes/lecture.md
[notes/text.md]: notes/text.md
[render.ts]: tools/render.ts
[scheme.xml]: notes/pandoc/scheme.xml
[src/compat/]: src/compat/
[src/lang/core.ss]: src/lang/core.ss
[src/sicp/]: src/sicp/
[src/sicp/]: src/sicp/
[style.css]: docs/style.css
[template.html]: notes/pandoc/template.html

[bem]: http://getbem.com/naming/
[Chez Scheme]: https://cisco.github.io/ChezScheme/
[clang-format]: https://clang.llvm.org/docs/ClangFormat.html
[Deno]: https://deno.land
[Guile]: https://www.gnu.org/software/guile/
[kate]: https://docs.kde.org/trunk5/en/kate/katepart/highlight.html
[KaTeX]: https://katex.org
[kde-scheme.xml]: https://github.com/KDE/syntax-highlighting/blob/70b56cf8b3d1a85e15d1e09aa8490e5183967de0/data/syntax/scheme.xml
[lectures]: https://ocw.mit.edu/courses/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video_galleries/video-lectures/
[luaposix]: https://github.com/luaposix/luaposix
[LuaRocks]: https://luarocks.org
[note]: https://mk12.github.io/sicp/exercise/language.html
[Pandoc Markdown]: https://pandoc.org/MANUAL.html#pandocs-markdown
[Pandoc]: https://pandoc.org
[R6RS Scheme]: http://www.r6rs.org
[Racket]: http://racket-lang.org
[sicp]: https://mitpress.mit.edu/sites/default/files/sicp/index.html
[skylighting]: https://github.com/jgm/skylighting
[static]: https://github.com/jgm/pandoc/issues/3986
[svgbob]: https://github.com/ivanceras/svgbob
[svgo]: https://github.com/svg/svgo
[the website]: https://mk12.github.io/sicp
[vnu]: https://validator.github.io/validator/
