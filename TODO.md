# New to do

- rewrite README, mentioning pandoc & BEM css
- figure out exercise chapter pages (there's never code between chapter & first section)
- BUILD EXERCISES DOCS! Then put proofs inline in the Scheme comments, using $$
- spell checker for Markdown, integrated in Makefile
- have special callout thing at end of text sections with exercise links
- attribute lecture quotes to Abelson/Sussman
- attribute some citations to Alan Perlis, etc.
- do citations for inline quotes and block quotes, linking to SICP website (need it for highlights since chapter heading is not specific enough!)
- make highlights links go directly to textbook/lecture
    - must do this because otherwise "external link" is a lie
- extract favorite quotes from blockquotes annotated with ::: highlight
- heart SVGs on highlights linked to quote.html
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
