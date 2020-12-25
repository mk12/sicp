# New to do

- distinguish h2/h3 more (especialy matttes in lecture, no numbers on h2)
- decide on frontmatter in TOC or not
- attribute lecture quotes to Hal/Gerald
+ settle on max-width
    + sticking with 800: want wide enough for SICP drawing; need code 80 chars; want blockquotes to look same as code, meaning same length
- fix intra-doc links ("earlier section")
+ remove scheme from all ```scheme... insert it auto, also on inline!!
- have special callout thing at end of text sections with exercise links
+ put highlight styles in standalone CSS
+ use /text/1#1.2 for section 1.2, /exercise/1#1.32 for Exercise 1.32, etc.
x note: GH pages allows omitting .html (without need for subfolder + index.html)
    x but want it to work locally, so sticking with .html links
x link quotes to the SICP website
    x can't link to precise quote locations so no point.
+ little hash anchors on headings, like github
+ color scheme https://coolors.co/98c1d9-2c497f-db5461-fbb5b1-38302e
- improve Scheme highlighting in docs
- BUILD EXERCISES DOCS! Then put proofs inline in the Scheme comments, using $$
- spell checker for Markdown, integrated in Makefile
- docs dark mode
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
