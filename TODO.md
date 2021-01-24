# New to do

- test in all browsers, without JS, without CSS
- exercises appendix for core.ss etc.
- deal with the OLD proofs, diagrams
- en/em dashes in comments
- go over all exercises and fix up
- HTML tables (Exercise 2.60)
- remove `<!-- DELETE -->` in text.md
- place spell in the right order for `make all`
- finish correcting spelling and adding ignores
- make spellc.m interactive
- perfection:
    - text: 1.1, 1.2
    - lecture: 1a, 1b
    - exercise: 1.1, 1.2
- fix `(+ 1 1) =?> [(* 1 1) "two"]` (getting `attempt to apply non-procedure #<void>`)
- document `capture-output` and `hide-output` in exercise.md
+ remove racket's patch-output hack by using `print-mpair-curly-braces`
+ mention chez fast, racket good error message in exercise.md
+ link to known issues in exercise.md
+ remove paste link from language.md
+ fix scheme linter
+ `=?>` operator instead of `in?`
+ fix assertion failure when building all docs
x html minifier
    x barely has any effect, size is from math spans
x reconsider → in lecture 6a
    x it's fine
+ automatic `&thinsp;` after code, `&hairsp;` between italic "r" and colon
+ decide on bold/italic for technicaly terms
    + just using _em_ for everything, simplest
+ fix bug in postprocess (not all inline code is scheme class)
+ implement spell checker using NSSpellChecker
+ better usage messages for C programs
+ mention `cons-stream` and `with-eval` in language.html
+ free lines from getline
+ clang-format C sources
+ decide on `eval`/`apply` headings
+ add all the exercise links mechanically
    + 3.5.2 has the most (example to look at)
+ highlight `=$>` and `=!>` differently
    + actually just `=!>`, in bold red. `=$>` is fine as is
+ don't highlight `div`
+ linkify `paste` IDs
    + https://stackoverflow.com/a/54219536
    + https://owickstrom.github.io/pandoc-emphasize-code/
    + injected HTML with `‹` and `›`, and omitted `"` since they are not necessary (`make validate` passes) and if I include pandoc coverts to `&quot;`
+ fix headings in language.html
+ fix h3 heading spacing (appears uneven after blockquote/code)
+ have special callout thing at end of text sections with exercise links
+ fixup highlights of `use`, `:1.2`, `?1.2`, etc.
+ finish writeup for exercise/language
+ finish writeup for exercise/index
+ exercise/language heading anchors
+ figure out the whole lua module symbol stuff
+ fix exercise link for 1.7.1, 1.7.2
x decide on styling for exercise headings (tabular numbers makes external link look bad)
    x only looked bad when they were lined up without any body
+ implement use-block styling
x links between text & exercise sections
    x no, too confusing
+ different sigil for text/lecture links (since :1.2.3 is exercise section)
+ implement exercise intralinking in lua filter
x problem: subsubsections like 1.2.3.1 do not have right-labels in text, but in exercise numbers are relevant due to imports; but subsubsections (h3) have no underline making number look weird
    x it's fine
+ respond on https://github.com/jgm/pandoc/issues/6651 with my katex solution
+ rewrite README
    + mention pandoc, katex, BEM css, deps.sh, emphasize no JS
+ update index.md in light of CC license
+ test in html5 validator https://html5.validator.nu/
+ BUILD EXERCISES DOCS! Then put proofs inline in the Scheme comments, using $$
+ fix highlighting of ~>
+ CC license for sicp Scheme files
+ attribute some citations to Alan Perlis, etc.
+ do citations for inline quotes and block quotes, linking to SICP website (need it for highlights since chapter heading is not specific enough!)
+ section 1.2.6 prevent $n$th from breaking apart
+ attribute lecture quotes to Abelson/Sussman
+ use ellipses without square brackets in quotes
+ pre-rendered math (https://github.com/jgm/pandoc/issues/6651)
    + 19/53 (35%) of current HTML files use katex (with exercises fraction will likely go down)
    + just $N$ takes 475 bytes; modest display eqn is 5KB (https://github.com/KaTeX/KaTeX/issues/2194)
    + see Simplenote "KaTeX" for more analysis; tldr pre-rendering is worth it
    + then do if code, `<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.11.1/katex.min.css">`
    + can use .INTERMEDIATE in make to start/stop js server (to avoid spawning node on EVERY math)
    + try https://deno.land instead of node! (so cool! so fast!)
    + `brew install luarocks`, `luarocks install luasocket`
    + pretty sure unix sockets are what I want https://stackoverflow.com/a/9476248
    + learning difference between stream/datagram unix sockets
    + https://stackoverflow.com/questions/9644251/how-do-unix-domain-sockets-differentiate-between-multiple-clients
    + trouble with Lua in pandoc: https://stackoverflow.com/a/56087697
    + `require "socket.unix"` failed. did `fenv (luarocks path)`, then got error about "Symbol not found: _lua_newuserdatauv"
    + found pandoc's version, 5.3: `pandoc --lua-filter <(echo 'print(_VERSION)') <<< ''`
    + `brew install luaver` and `luaver install 5.3.6` (latest 5.3 https://www.lua.org/versions.html)
    + 2 problems:
        + lua timeout 0 returns immediately if there is nothing ... what I want is blocking recv 1<=s<=1024
            + can either prefix with sizes, or switch to newline endings
            + went with newlines
        + await in the katex.ts loop prevents handling concurrently
            + made fire and forget (well, except catch)
    + working except OS error 22 when -j4 or higher
    + hammer.ts works so I suspect maybe luasocket is buggy
    + nope: found culprit (maybe) https://github.com/denoland/deno/blob/master/cli/tests/unit/net_test.ts#L159
    + confirmed on discord that you should `call conn.close()`
    + found luasocket is 3.0-rc1 from 2013 despite many recent commits on GitHub
    + never mind, GH tag is weird, `luarocks --lua-dir=/usr/local/opt/lua@5.3 show luasocket` shows 3.0rc1-2 (most recent non-dev release, from last year)
    + discussion about pandoc and lua modules https://github.com/jgm/pandoc/issues/4230
    + lua filters can't use modules with .so files!! https://github.com/jgm/pandoc/issues/3986
    + the situation:
        + luasocket.unix likely buggy
        + pandoc can only use lua modules without .so (that's why cqueues doesn't work)
    + luaposix works and is documented http://luaposix.github.io/luaposix/
    + answered SO question: https://stackoverflow.com/a/65569899
    + remember, can't host katex.min.css b/c it references fonts relatively
    + font size issue: default 1.21em lines up x-height but capitals are really big
+ docgen should have exit status 1 if pandoc fails
+ italicize output or just use ; => comments
    + e.g. lecure 5a pt3, text 2.1.1
    + decided to use `=> 123` like in exercises BUT: always at start of line, never inlined into previous, AND not in cases where each line of the code block is showing a reduction
+ fix soureCode C thing (just removed C code since it was inaccurate; &x is not box since it does not allocate) (https://github.com/jgm/pandoc/issues/4386)
+ reconsider inline code single-word highlighting
    + nice for parity, but:
        + blue functions look like links. and even if I changed color...
        + too distracting in paragraphs with lots of little `things`
            + e.g. text 3.3.1
        + does not help parsing the way code blocks do -- nothing to parse!
        + single word IS nice for the ss underline
        + problem: "the old `car` is unreachable". blue is bad here
+ add guillements in lecture.md
+ with scrolling code, full 80 chars scrolls tiny bit. fix.
x indication that code/math scroll
    x considered gradient fade-out on right. looks bad.
    x StackOverflow keeps scrollback with ::webkit-scrollbar. too hard
+ consistent CSS property ordering https://css-tricks.com/poll-results-how-do-you-order-your-css-properties/
+ fix responsiveness (broken; resizing overflows right edge)
    + bisected by deleting nodes in inspector; culprit is site header
    + with only header, isn't even centred on wide page!
    + ... removed *everything* in body, still weird size
    + OK, reproduced on text/1/2.html
    + dumb! it was just the display math! same with code
    + but before header numbers were overflowing too. Can't reproduce that.
    + make math and code scroll after all.
    + code definitely scroll (and now way to prevent Safari from hiding scroll bars, oh well)
    + math tried before, had vertical scroll bar issue https://stackoverflow.com/q/6421966, https://talk.observablehq.com/t/vertical-scroll-bars-on-display-katex/2304/4, https://github.com/KaTeX/KaTeX/issues/327
    + can fix by adding a tiny bit of padding
    + (would prefer scaling down the math but there's no CSS way to scale text to fit a container)
+ more indentation for nested lists (look at github's indentation)
+ replace % in sicp URLs with %25
+ aria-hidden instead of alt="" for SVGs
+ remove `<span class="sc">»</span>`
+ fix scheme.xml numbers
    + `++` means this https://stackoverflow.com/a/4489585
+ remove empty `<a>` tags, classes, spans, etc. in source listings 
    + need to stream pandoc output and modify it in docgen
    + in theory, could be most optimal to write pandoc input in 1 thread and transform its output in another
    + in pratice, I suspect docgen is so fast that it finishes generation before pandoc even gets a chance to open the output file
    + timing: docgen takes 10ms on docs/text/highlight.html (slowest as it scans all text.md)
    + so not worth pipelining this as it will save a few ms at best -- and that's only in the ideal conditions where thread creation + pandoc latency to first byte of output is less than 10ms
    + brought 741450B -> 658270B html, 83KB saving!
x consider if any em should be rem
+ fix spacing between arrows and text
+ decide whether to use article
+ figure out document outline, logo h1 etc.
+ big numbers for chapters
+ fix margins on .nubmer
+ ensure HTML formatting no space in inlines
x use contextual class names instead of SVG file name based
    x nah since the class="..." are in SVG files
+ use grid instead of flexbox for narrow sitenav, and add border-top
+ prettify html output (indentation of SVGs etc)
x use ul in navs
    x if I could use display:contents, ul would be a cinch
    x it's 90%+ but with accessibility bugs ... the very reason to use it
    x going to stick with plain <a>s for simplcity and less bloat
+ fixup lecture.md blurb wording
+ redo pixel-perfect vertical-align
+ make highlights links go directly to textbook/lecture
    + must do this because otherwise "external link" is a lie
+ extract favorite quotes from blockquotes annotated with ::: highlight
+ interlink all highlights
+ has_pagenav->pagenav? Also sort out svg names once and for all
+ transform symbols in SVG!
+ decide on ID naming convention
    + in past I've done .class-name, #IdName, but heading IDs don't follow this
    + stick with kebab-case
+ rename MarkdownScanner.heading
+ refactor docgen with HighlightScanner
+ more links in LICENSE.md
+ make pandoc process die when docgen dies
+ pandoc filter to allow #1, #1.1, #1.1.1, #1a, etc. and convert to 1/1.html#1.1.1 etc.
+ test site with stylesheet disabled
+ remove padding in SVGs, make pixel perfect, attributes/css
+ decide on SVG attribute vs CSS
    + definitely put width/height otherwise disabling CSS creates huge gaps
    + pro of SVG: previewing SVG file is correct
+ rename quote.html to highlight.html
+ add external link icon to indicate header link affordance (and not _blank, just to indicate; precedent here in Wikipedia)
+ linkify all headers
+ title on all `<a>` tags for accessibility
+ `<cite>` insitead of `<em>` for book title
+ test with VoiceOver
+ add skip to main content link
+ fix "#" links in light of media query/body padding
x Note: no need to link between exercise/text other than just having lists of exercise links in text where they appear
+ nav element for ToCs
+ consistent char[] sizes
+ DRY up docgen.c templates
+ change small to span and add space for Safari Reader
+ userspace buffering to avoid so many write(2) calls
+ revamp colors AGAIN, and fix underlines for meta, and revisit 1-word highlight
+ fix contrast in code highlighting (revamp colors)
+ fix `<formal parameters>` etc.
+ use config.yml for pandoc and short options for better Make output
+ optimize SVGs with https://jakearchibald.github.io/svgomg/
    + note: not using `<use>` for arrows since they're so small
+ test in Safari, Firefox, Chrome
+ improve accessibility, alt/title text, main tag etc.
+ improve Scheme highlighting in docs (and support dark mode)
+ decide on en/em dashes (going with closed-set em)
x if quotes always included in notes, maybe quote.md should just be references/line numbers/something (and then maybe place back into the main file)
    x too much work
+ dark mode
+ fix monospace in heading, weird spacing
+ fix italics collisions
    + only a problem with "r", so manually added `&hairsp;` in 1.1.5
+ fix intra-doc links ("earlier section")
+ responsive
x text-rendering: optimizeLegbility
    x seems to make no difference on helvetica
+ SVG for arrows in pagenav
+ tighten leading on headings without changing margins
+ fix chapter/section numbers when heading wraps
+ distinguish h2/h3 more (especialy matttes in lecture, no numbers on h2)
x decide on frontmatter in TOC or not
    x no, looks bad since it doesn't have number
+ finish homepage
+ licenses (MIT + CC-BY-SA)
+ settle on max-width
    + sticking with 800: want wide enough for SICP drawing; need code 80 chars; want blockquotes to look same as code, meaning same length
+ remove scheme from all ```scheme... insert it auto, also on inline!!
+ put highlight styles in standalone CSS
+ use /text/1#1.2 for section 1.2, /exercise/1#1.32 for Exercise 1.32, etc.
x note: GH pages allows omitting .html (without need for subfolder + index.html)
    x but want it to work locally, so sticking with .html links
x link quotes to the SICP website
    x can't link to precise quote locations so no point.
+ little hash anchors on headings, like github
+ color scheme https://coolors.co/98c1d9-2c497f-db5461-fbb5b1-38302e
+ drawing of wizard book yin/yang for index.hml
x switch to netlify to avoid checking in docs?
    x No, GitHub pages is simpler. No containers, no slow builds. Just HTML.
+ make check for shellcheck (needs to be distinct from lint for vscode task error recognition)
+ change install-foo-package to foo-pkg
+ fix BUG in chapter-3
+ lint import order
+ make a linter
+ change indentation from two to one spaces unless let/lambda/define/etc.
+ change =/> (looks like does NOT equal) to =$>
+ move study markdown into repo
+ special arrow for output/lines
+ remove debug stuff from core.ss
+ remove --slow
+ assert raises
+ remove make-parameter/parameterize from sicp

# Considered

Assert no exception on `=>`?
no: (1) inconsistent `(foo)` and `(foo) => 'ok`, and will slow down compilation more

# To Do

+ main.scm to run stuff
+ --slow flag

need:
+ use stuff from later chapter
+ sometimes don't override: sqrt
+ sometimes override: real-part

2 approaches:
A semantic (exercise 1.1 ...)
B ability (scope), (check), rest in comments

would like main.scm to show names of exercises...
but B is more flexible.


or:

(section foo)

(exercise 1.1)
(check)

issues:
- defines in check or outside?
- one big check, or many small?
- use (f x) in next section (but not globally)
- define same procedure multiple ways
