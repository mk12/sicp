These are my solutions to the exercises in <cite>[Structure and Interpretation of Computer Programs][sicp]</cite>. The code is written in a language based on [R6RS Scheme][] that provides lightweight modules and assertions, allowing each section to explicitly declare its dependencies and run unit tests. The language is described more in the [next section][].

This project supports three Scheme implementations: [Chez Scheme][], [Guile][], and [Racket][]. For each one there is a [compatibility shim][] exposing some features that are not part of R6RS under a common interface.

These webpages are [generated][] directly from source code.

[sicp]: https://mitpress.mit.edu/sites/default/files/sicp/index.html
[next section]: language.html "A Note on the Language"
[R6RS Scheme]: http://www.r6rs.org
    "The Revised(6) Report on the Algorithmic Language Scheme"
[Chez Scheme]: https://cisco.github.io/ChezScheme/ "Chez Scheme"
[Guile]: https://www.gnu.org/software/guile/ "GNU Guile"
[Racket]: http://racket-lang.org "Racket programming language"
[compatibility shim]: https://github.com/mk12/sicp/blob/master/src/compat
[generated]: https://github.com/mk12/sicp#website

# Note on the Language

The code on this website is written in a language based on [R6RS Scheme][]. The language is [implemented][] with macros, and provides two features: modules and assertions.

[R6RS Scheme]: http://www.r6rs.org
    "The Revised(6) Report on the Algorithmic Language Scheme"
[implemented]: https://github.com/mk12/sicp/blob/master/src/lang/core.ss

## Modules

While R6RS provides a module system, using them for hundreds of sections and exercises is impractical. Their verbose syntax and indentation would distract from the flow of the chapter. We also need the flexibility to control module evaluation, for example to only run a subset of tests. This leads us to a custom solution:

```
(Chapter :1 "Chapter Title")

(define x 1)

(Section :1.1 "Section Title")

(define w (+ x y z)) ; ERROR: x, y, and z are undefined

(Exercise ?1.1)

(define y 2)
(define z 3)
```

Although every expression is at the top level, this actually defines three separate modules. Each module has a unique identifier, beginning with the ":" sigil for chapters and sections and the "?" sigil for exercises. Modules do not nest, so `:1` and `:1.1` are equal siblings rather than parent and child.

### Imports

We can import from other modules like so:

```
(Chapter :1 "Chapter Title")

(define x 1)

(Section :1.1 "Section Title"
  (use (:1 x) (?1.1 y z)))

(define w (+ x y z)) ; ok
(set! x 10) ; only affects :1.1

(Exercise ?1.1)

(define y 2)
(define z 3)
```

Here, `:1.1` imports `x` from `:1`, and `y` and `z` from `?1.1`. This implies that `:1.1` must be evaluated after the other two modules, not in source order. The test runner topologically sorts modules to achieve this, and fails if there are any cycles. Importing from later modules is occasionally very useful. For example, [](:2) depends heavily on two-dimensional tables, which are not implemented until [](:3.3.3.3).

You will not see expressions like `(Chapter ...)` and `(Section ...)` elsewhere on this website because they are converted to HTML headings. Likewise, the `(use ...)` blocks are converted to compact HTML lists set in a smaller font.

To avoid confusion, shadowing imports is not allowed:

```
(Exercise ?1.1
  (use (:1.1 x)))

(define x 2) ; ERROR: imported x is shadowed by local definition
```

Modules get turned into procedures, which do not allow mixing definitions with other expressions. Therefore, all top-level variables get hoisted to the top. To illustrate:

```
(Exercise ?1.1)

(display x) ; prints #<void> in chez
(display y) ; ERROR: y is undefined
(define x 1)
```

### Pasting

The language provides one more feature for code reuse: _pasting_. Modules can unhygienically paste code from an earlier module in the same file: 

```
(Section :1 "Original")

(define a 1)
(define (inc x) (+ x a))
(inc 42) ; 43

(Section :2 "Paste")

(define a -1)
(paste (:1 inc))
(inc 42) ; 41
```

If `:2` had imported `inc` instead, the result would be 43 in both cases since `a` is resolved lexically. Using `paste` is inelegant, but preferable to duplicating code. It is often useful when an exercise asks to make an adjustment to earlier code and there is no dynamic dispatch to hook into.

### Packages

Starting in [](:2), many modules use [data-directed programming](@2.4.3) to dispatch based on operation names and type tags. Modules define _packages_ (procedures ending in `-pkg`) that install operations in the global table. For example:

```
(define (real-part z) (apply-generic 'real-part z))

(define (rectangular-pkg)
  (put 'real-part '(rectangular) ...)
  ...)
(define (polar-pkg)
  (put 'real-part '(polar) ...)
  ...)

(using rectangular-pkg polar-pkg)

; tests go here
```

The `using` procedure defined in [](:2.4.3) resets the global table and installs the given packages. There is no automatic tracking of dependencies, so `using` calls must list everything explicitly. This package system is not part of the language, but it's worth explaining here because so many modules use it.

## Assertions

As with modules, standard techniques for assertions are too heavyweight. The mere word "assert" is too verbose for our use case. Instead, the language provides four assertion operators that work at the top level: `=>`, `~>`, `=$>`, and `=!>`. They report detailed information when they fail, including the actual result, expected result, and line number.

### Exact

The `=>` operator is by far the most common. It asserts that the left-hand side is equal to the right-hand side, using `equal?`. For example:

```
(+ 1 1) => 2
```

It can also be chained to assert that several expressions are equal, while ensuring that each expression is evaluated only once:

```
(fact 3)
=> (* 3 (fact 2))
=> (* 3 (* 2 (fact 1)))
=> (* 3 (* 2 1))
=> (* 3 2)
=> 6
```

When `(fact 3) => (fact 5)` fails, the output looks like this:

<pre><code class="blockcode"><!--
--><span class="bo">path/to/file.ss:123:1: assertion failed</span>
left: <span class="fu">(fact 3)</span>
=> <span class="cn">6</span>

right: <span class="fu">(fact 5)</span>
=> <span class="cn">120</span>

test result: <span class="er">FAIL</span>. 0 passed; 1 failed; 0 filtered out
</code></pre>

### Inexact

The `~>` operator asserts that floating-point numbers are approximately equal using an absolute tolerance of $10^{-10}$. For example:

```
(* 4 (atan 1)) ~> 3.141592653589793
```

Like `=>`, it can be chained. Each item is compared to the previous one, not to the first.

When `(* 4 (atan 1)) ~> 3.14` fails, the output looks like this:

<pre><code class="blockcode"><!--
--><span class="bo">path/to/file.ss:123:1: assertion failed</span>
left: <span class="fu">(* 4 (atan 1))</span>
=> <span class="cn">3.141592653589793</span>

right: <span class="fu">3.14</span>
=> <span class="cn">3.14</span>

delta: <span class="cn">0.0015926535897929917</span> > 1e-10

test result: <span class="er">FAIL</span>. 0 passed; 1 failed; 0 filtered out
</code></pre>

### Output

The `=$>` operator captures standard output. For example:

```
(display "hello") =$> "hello"
```

If the right-hand side is a list, it is not evaluated but instead treated as a list of lines. By convention these lists use square brackets, which are interchangeable with parentheses in R6RS but not used anywhere else in this project.

```
(define (foo)
  (display "hello")
  (newline)
  (display "world"))

(foo) =$> ["hello" "world"]
```

Newlines occurring at the beginning, end, or after another newline are ignored:

```
(display "\nOne\n\nTwo\n\n\nThree\n") =$> ["One" "Two" "Three"]
```

When `(display "pong\nping") =$> ["ping" "pong"]` fails, the output looks like this:

<pre><code class="blockcode"><!--
--><span class="bo">path/to/file.ss:123:1: assertion failed</span>
left: <span class="fu">(display "pong\nping")</span>
=$> [<span class="yl">"pong"</span>
     <span class="yl">"ping"</span>]

right:
=$> [<span class="yl">"ping"</span>
     <span class="yl">"pong"</span>]

test result: <span class="er">FAIL</span>. 0 passed; 1 failed; 0 filtered out
</code></pre>

### Error

The `=!>` operator asserts that an error will occur. For example:

```
(error 'foo "oops" 1 2) =!> "foo: oops: 1 2"
```

The assertion passes if the right-hand side occurs as a substring in the representation `«who»: «message»: «irritants»`. Thus, `(f) =!> ""` passes as long as `(f)` raises _any_ error. This operator works best for errors raised by user code, as opposed to system errors. For example, `(/ 1 0)` fails in all Schemes, but they all use different error messages.

When `(+ 1 2) =!> "disaster"` fails, the output looks like this:

<pre><code class="blockcode"><!--
--><span class="bo">path/to/file.ss:123:1: assertion failed</span>
left: <span class="fu">(+ 1 2)</span>
=> <span class="cn">3</span>

right:
=!> ... <span class="co">disaster</span> ...

test result: <span class="er">FAIL</span>. 0 passed; 1 failed; 0 filtered out
</code></pre>

When `(error 'foo "catastrophe" 1) =!> "disaster"` fails, the output looks like this:

<pre><code class="blockcode"><!--
--><span class="bo">path/to/file.ss:123:1: assertion failed</span>
left: <span class="fu">(error 'foo "catastrophe" 1)</span>
=!> <span class="co">foo: catastrophe: 1</span>

right:
=!> ... <span class="co">disaster</span> ...

test result: <span class="er">FAIL</span>. 0 passed; 1 failed; 0 filtered out
</code></pre>
