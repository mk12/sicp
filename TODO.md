# New to do

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
