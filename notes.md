# Dedication

> What's in your hands, I think and hope, is intelligence: the ability to see the machine as more than when you were first led up to it, that you can make it more. (11)

# Foreword

- Three foci: the human mind, collections of computer programs, and the computer.
- Idioms form "an arsenal of standard program structures of whose correctness we have become sure" (13).
- Algorithms are programs that perform a precise mathematical function (optimized for execution time and data storage).

> Lisp is for building organisms -- imposing, breathtaking, dynamic structures built by squads fitting fluctuating myriads of simpler organisms into place. (15)

> It is better to have 100 functions operate on one data structure than to have 10 functions operate on 10 data structures. (15)

# Preface

- Computer language: a novel formal medium for expressing ideas about methodology.

> Thus, programs must be written for people to read, and only incidentally for machines to execute. (19)

> Underlying our approach to this subject is our conviction that "computer science" is not a science and that its significance has little to do with computers. (20)

> Mathematics provides a framework for dealing precisely with notions of "what is." Computation provides a framework for dealing precisely with notions of "how to." (20)

# Building Abstractions with Procedures

- A computational process evolves to manipulate data.
- The evolution is controlled by a program, a pattern of rules.
- Well-designed computational systems are modular.
- This book uses the Scheme dialect of Lisp.
- Lisp represents procedures as data.

## The elements of programming

There are three mechanisms for combining simple ideas to form more complex ideas in every powerful programming language:

- primitive expressions,
- means of combination,
- means of abstraction.

Programming deals with _procedures_ and _data_ (which are almost the same thing in Lisp). Procedures manipulate data.

### Expressions

- The REPL reads an expression, evaluates it, prints the result, and repeats.
- A number is one kind of primitive expression.
- An application of a primitive procedure is a compound expression.
- Combination: list of expressions inside parentheses to denote procedure application.
- The first element is the operator; the rest are the operands.
- The value of the combination is the result of applying the value of the operator to the value of the operands.
- Lisp combinations use prefix notation.
- Combinations can be nested: an operator or operand can itself be another combination.

### Naming and the environment

- Scheme names things with the `define`. This is the simplest means of abstraction.
- The name-value pairs are stored in an _environment_.

### Evaluating combinations

- To evaluate a combination, do the following:
1.1. Evaluate the subexpressions of the combination.
1.2. Apply the procedure (value of left more subexpression, the operator) to the arguments (values of other subexpressions, the operands).
- Before evaluating a combination, we must first evaluate each element inside it.
- Evaluation is recursive in nature -- one of its steps is invoking itself.
- The evaluation of a combination can be represents with a tree.
- Recursion is a powerful technique for dealing with hierarchical, tree-like objects.
- To end the recursion, we stipulate the following:
1.1. Numbers evaluate to themselves.
1.2. Built-in operators evaluate to machine instruction sequences.
1.3. Names evaluate to the values associated with them in the environment.
- Rule 2 is a special case of rule 3 if we consider the arithmetic operators to be names in the environment.
- Evaluating `(define x 3)` does not apply `define` to two arguments; this is not a combination.
- Exceptions such as these are _special forms_. Each one has its own evaluation rule.

> Syntactic sugar causes cancer of the semicolon. (Alan Perlis)

### Compound procedures

- Procedure definitions are very powerful for abstraction.
- A squaring procedure: `(define (square x) (* x x))`.
- This is a compound procedure given the name _square_.
- The general form of a procedure definition is `(define (<name> <formal parameters>) <body>)`.
- If the body contains more than one expression, each is evaluated in sequence and the value of the last one is returned.

### The substitution model for procedure application

This is the substation model:

> To apply a compound procedure to arguments, evaluate the body of the procedure with each formal parameter replaced by the corresponding argument.

An example of procedure application:

```
(f 5)
(sum-of-squares (+ 5 1) (* 5 2))
(sum-of-squares 6 10)
(+ (square 6) (square 10))
(+ 36 100)
136
```

#### Applicative order versus normal order

- That example used _applicative order_: evaluate everything first, then apply the procedure to the arguments.
- With _normal order_, operands are substituted in the procedure unevaluated. Only when it reaches primitive operators do combinations reduce to values.

An example of normal order procedure application:

```
(f 5)
(sum-of-squares (+ 5 1) (* 5 2))
(+ (square (+ 5 1)) (square (* 5 2)))
(+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
(+ (* 6 6) (* 10 10))
(+ 36 100)
136
```

- In this example, normal order causes a few combinations to be evaluated multiple times.
- Applicative: evaluate the arguments and then apply.
- Normal: fully expand and then reduce.

### Conditional expressions and predicates

- To make more useful procedures, we need to be able to make tests and perform different operations accordingly.
- We do _case analysis_ in Scheme using `cond`.
- Those conditional expressions work by testing each predicate. The consequent expression of the first clause with a true predicate is returned, and the other clauses are ignored.
- A predicate is an expression that evalues to true or false, or a procedure that returns true or false.
- The symbol `else` can be used as the last clause -- it will always evaluate to true.
- The `if` conditional can be used when there are two cases.
- Logical values can be combined with `and`, `or`, and `not`. The first two are special forms, not procedures.

### Example: Square roots by Newton's method

> But there is an important difference between mathematical functions and computer procedures. Procedures must be effective. (48)

- In mathematics, you can say "the square root of $x$ is the nonnegative $y$ such that $y^2 = x$." This is not a procedure.
- Mathematical functions describes things (declarative knowledge); procedures describe how to do things (imperative knowledge).
- Declarative is _what is_, imperative is _how to_.

### Procedures as black-box abstractions

- Each procedure in a program should accomplish and identifiable task that can be used as a module in defining other procedures.
- When we use a procedure as a "black box," we are concerned with _what_ it is doing but not _how_ it is doing it.
- This is called procedural abstraction. Its purpose is to suppress detail.

> A user should not need to know how the procedure is implemented in order to use it. (54)

#### Local names

- The choice of names for the procedure's formal parameters should not matter to the user of the procedure.
- Consequentially, the parameter names must be local to the body of the procedure.
- The name of the formal parameter doesn't matter; it is called a _bound variable_. The procedure _binds_ its formal parameters.
- If a variable is not bound, it is _free_.
- The expressions in which a binding exists is called the _scope_ of the name. For parameters of procedures, this is the body.
- Using the same name for a bound variable and an existing free variable is called _capturing_ the variable.
- The names of the free variables _do_ matter for the meaning of the procedure.

#### Internal definitions and block structure

- Putting a definition in the body of a procedure makes it local to that procedure. This nesting is called _block structure_.
- Now we have two kinds of name isolation: formal parameters and internal definitions.
- By internalizing auxiliary procedures, we can often eliminate bindings by allowing variables to remain free.
- Lexical scoping: free variables in a procedure refer to bindings in enclosing procedure definitions.

## Procedures and the processes they generate

> To become experts, we must learn to visualize the processes generated by various types of procedures. Only after we have developed such a skill can we learn to reliably construct programs that exhibit the desired behaviour. (59)

- A procedure is a pattern for the _local evolution_ of a computation process: how one stage is built on the previous.
- The global behaviour of a computational process is much harder to reason about.
- Processes governed by different types of procedures generate different "shapes" of evolution.
- There are two important resources that computational processes consume: time and space.

### Linear recursion and iteration

- The factorial of $N$ is defined as the product of the integers on the interval $[1,N]$.
- The naive _recursive_ implementation creates a curved shape:

```
(factorial 4)
(* 4 (factorial 3))
(* 4 (* 3 (factorial 2)))
(* 4 (* 3 (* 2 (factorial 1))))
(* 4 (* 3 (* 2 1)))
(* 4 (* 3 2))
(* 4 6)
24
```

- The _iterative_ implementation maintains a running product and multiplies the numbers from 1 to $N$ to it.
- This creates a shape with a straight edge:

```
(factorial 4)
(fact-iter 1 1 4)
(fact-iter 1 2 4)
(fact-iter 2 3 4)
(fact-iter 6 4 4)
(fact-iter 24 5 4)
24
```

- Both compute the same mathematical function, but the computational processes evolve very differently.
- The first one is a _linear recursive process_. The chain of deferred operations causes an expansion (as more operations are added) and a contraction (as the operations are performed).
	- The interpreter must keep track of all these operations.
	- It is a _linear_ recursive process because the information it must keep track of (the call stack) grows linearly with $N$.
- The second is a _linear iterative process_. It is iterative because it does not grow and shrink.
	- It is summarized by a fixed number of state variables and a rule to describe how they should update and when the process should terminate.
	- It is a _linear_ iterative process because the number of steps grows linearly with $N$.
- In the iterative process, the variables provide a complete description of the state of the process at any point.
- In the recursive process, their is "hidden" information that makes it impossible to resume the process midway through.
- The longer the chain of deferred operations, the more information must be maintained (in a stack, as we will see).
- A recursive _procedure_ is simply a procedure that refers to itself directly or indirectly.
- A recursive _process_ refers to the evolution of the process described above.
- A recursive procedure can generate an iterative process in Scheme thanks to tail-call optimization. In other languages, special-purpose looping constructs are needed for this.

### Tree recursion

- With tree recursion, the procedure invokes itself more than once, causing the process to evolve in the shape of a tree.
- The naive Fibonacci procedure calls itself twice each time it is invoked, so each branch splits into two at each level.

> In general, the number of steps required by a tree-recursive process will be proportional to the number of nodes in the tree, while the space required will be proportional to the maximum depth of the tree. (67)

- The iterative implementation of the Fibonacci procedure is vastly more efficient in space and in time.

#### Example: Counting change

Let $f(A,N)$ represent the number of ways of changing the amount A using $N$ kinds of coins. If the first kind of coin has denomination $N$, then $f(A,N) = f(A,N−1) + f(A−D,N)$. In words, there are two situations: where you do not use any of the first kind of coin, and when you do. The value of $f(A,N−1)$ assumes we don't use the first kind at all; the value of $f(A−D,N)$ assumes we use one or more of the first kind.

That rule and a few degenerate cases is sufficient to describe an algorithm for counting the number of ways of changing amounts of money. We can define it with the following piecewise function:

$$f(A,N) = {(1, if A=0","),(0, if A < 0","),(0, if N=0","),(f(A,N-1)+f(A-D,N), if A > 0 and N > 0.):}$$

Like Fibonacci, the easy tree-recursive implementation involves a lot of redundancy. Unlike it, there is no obvious iterative solution (it is possible, just harder). One way to improve the performance of the tree-recursive process is to use _memoization_ (maintaining a lookup table).

### Orders of growth

- Some processes consume more or less computational resources than others.
- We compare this using _order of growth_, a gross measure of the resources required by a process as the inputs becomes larger.
- Let $n$ be a parameter that measures the size of a problem -- it could be the input itself, the tolerance, the number of rows in the matrix, etc. There are many properties that $n$ can measure.
- Let $R(n)$ be the amount of resources the process requires for a problem of size $n$. This could be time, space (amount of memory), number of registers used, etc.
- We say that $R(n)$ has order of growth $Θ(f(n))$, or $R(n) = Θ(f(n))$,
if there are positive constants $A$ and $B$ independent of $n$ such that $Af(n) ≤ R(n) ≤ Bf(n)$ for any sufficiently large value of $n$.
- The value $R(n)$ is sandwiched between $Af(n)$ and $Bf(n)$.
- The linear recursive process for computing factorials had $Θ(n)$ time and $Θ(n)$ space (both linear), whereas the linear iterative process had $Θ(1)$ space (constant).
- The order of growth is a crude description of the behaviour of a process.
- Its importance is allowing us to see the _change_ in the amount of resources required when you, say, increment $n$ or double $n$.

### Exponentiation

One way to calculate b to the nth power is via the following recursive definition:

$$b^0 = 1, qquad b^n = b * b^(n-1)$$.

In words, multiply the base to itself $n$ times. A faster method is to use successive squaring:

$$b^n = {((b^(n//2))^2, if n\ "is even";),(b * b^(n-1), if n\ "is odd".):}$$

### Greatest common divisors

- The GCD of integers $a$ and $b$ is the largest integer that divides both $a$ and $b$ with no remainder. For example, $"GCD"(16,28) = 4$.
- Efficient algorithm uses $"GCD"(a,b) = "GCD"(b,a % b)$.
- For example, we can reduce `(gcd 206 40)` as follows:

```
(gcd 206 40)
(gcd 40 6)
(gcd 6 4)
(gcd 4 2)
(gcd 2 0)
2
```

- This always works: you always get a pair where the second number is zero, and the other number is the GCD of the original pair.
- This is called _Euclid's Algorithm_.
- Lamé's Theorem: If Euclid's Algorithm requires $k$ steps to compute the GCD of some pair, then $min{a,b} ≥ "Fib"(k)$.

### Example: Testing for primality

#### Searching for divisors

- One way to test for primality is to find the number's divisors.
- A number is prime if and only if it is its own smallest divisor.

#### The Fermat test

The Fermat test is a $Θ(log(n))$ primality test based on Fermat's Little Theorem:

> If $n$ is a prime number and a is any positive integer less than $n$, then a raised to the $n$th power is congruent to a modulo $n$.

The test works like this:

1. Given a number $n$, pick a random number $a < n$ and calculate $a^n % n$.
2. Fail: If the result is not equal to $a$, then $n$ is not prime.
3. Pass: If the result is equal to $a$, then $n$ is likely prime.
4. Repeat. The more times the number passes the test, the more confident we are that $n$ is prime. If there is a single failure, $n$ is certainly not prime.

#### Probabilistic methods

- Most familiar algorithms compute an answer that is guaranteed to be correct. Not so with the Fermat test.
- We can make the probability error in our primality test as small as we like simply by running more tests -- except for Carmichael numbers.
- If $n$ passes the test for one random value of a, the chances are more than 50% that $n$ is prime.
- Probabilistic algorithms: algorithms for which one can prove that the chance of error becomes arbitrarily small.

> Numbers that fool the Fermat test are called Carmichael numbers, and little is known about them other than that they are extremely rare. There are 255 Carmichael numbers below 100,000,000. The smallest few are 561, 1105, 1729, 2465, 2821, and 6601. In testing primality of very large numbers chosen at random, the chance of stumbling upon a value that fools the Fermat test is less than the chance that cosmic radiation will cause the computer to make an error in carrying out a "correct" algorithm. Considering an algorithm to be inadequate for the first reason but not for the second _illustrates the difference between mathematics and engineering_.

## Formulating abstractions with higher-order procedures

> We have seen that procedures are, in effect, abstractions that describe compound operations on numbers independent of the particular numbers. (74)

- We could always use `(* x x x)` instead of using a cube procedure, but this would be a disadvantage.

> One of the things we should demand from a powerful programming language is the ability to build abstractions by assigning names to common patterns and then to work in terms of the abstractions directly. (75)

- Even with procedures, we are still limiting ourselves if we only ever let parameters be numbers.
- To abstract more general programming patterns, we need to write precedes that take other procedures as arguments and return new procedures.
- These are called _higher-order_ procedures.

### Procedures as arguments

Procedures that compute a sum are all similar. They are all based on the following template:

```
(define (<name> a b)
  (if (> a b)
    0
    (+ (<term> a)
       (<name> (<next> a) b))))
```

This is a useful abstraction, just as sigma notation in math is useful because the summation of a series is so common.

> The power of sigma notation is that it allows mathematicians to deal with the concept of summation itself rather than only with particular sums. (77)

### Constructing procedures using lambda

`lambda` creates anonymous procedures. They are just like the procedures created by `define`, but without a name: `(lambda (<formal-parameters>) <body>)`.

A lambda expression can be used as the operand in a combination. It will be evaluated to a procedure and applied to the arguments (the evaluated operands). The name comes from the λ-calculus, which was introduced by Alonzo Church.

#### Using let to create local variables

- We often need local variables other than the ones that have been bound as formal parameters.
- We can do this with a lambda expression that takes the local variables as arguments, but this is so common that there is a special `let` form that does it.

The general form of a let-expression is

```
(let ((<var1> <exp1>)
      (<var2> <exp2>)
      ...
      (<varn> <expn>))
  <body>)
```

This is just syntactic sugar for

```
((lambda (<var1> <var2> ... <varn>)
   <body>)
 <exp1>
 <exp2>
 ...
 <expn>)
```

- The scope of a variable in a let-expression is the body.
- This allows variables to be bound as locally as possible.
- The variables in the let-expression are parallel and independent. They cannot refer to each other, and their order does not matter.
- You can use let-expressions (`let`, `let*`, and `letrec`) instead of internal definitions (block structure).

### Procedures as general methods

So far, we have seen

- compound procedures that abstract patterns of numerical operators (mathematical functions), independent of the particular numbers;
- higher-order procedures that express a more powerful kind of abstraction, independent of the procedures involved.

Now we will take it a bit further.

#### Finding roots of equations by the half-interval method

- The _half-interval_ method: a simple but powerful technique for finding the solutions to $f(x) = 0$.
- Given $f(a) < 0 < f(b)$, there must be at least one zero between $a$ and $b$.
- To narrow it down, we let $x$ be the average of $a$ and $b$, and then replace either the left bound or the right bound with it.

#### Finding fixed points of a function

- A number $x$ is a _fixed point_ of a function if $f(x) = x$.
- In some cases, repeatedly applying the function to an initial guess will converge on the fixed point.
- The procedure we made earlier for finding square roots is actually a special case of the fixed point procedure.

### Procedures as returned values

Passing procedures as arguments gives us expressive power; Returning procedures from functions gives us even more. For example, we can write a procedure that creates a new procedure with average damping:

```
(define (average-damp f)
  (lambda (x) (average x (f x))))
```

If we use `average-damp` on `square`, we actually get a procedure that takes the sum of the numbers from 1 to n:

```
((average-damp square) 10)
55
(+ 1 2 3 4 5 6 7 8 9 10)
55
```

> In general, there are many ways to formulate a process as a procedure. Experienced programmers know how to choose procedural formulations that are particularly perspicuous, and where useful elements of the process are exposed as separate entities that can be reused in other applications.

#### Newton's method

The square-root procedure we did earlier was a special case of Newton's method.  Given a function $f(x)$, the solution to $f(x) = 0$ is given by the fixed point of

$$x ↦ x − (f(x))/(f'(x))$$.

Newton's method converges very quickly -- much faster than the half-interval method in favourable cases. We need a procedure to transform a function into its derivative (a new procedure). We can use a small dx for this:

$$f'(x) = (f(x+dx) - f(x))/dx$$.

This translates to the following procedure:

```
(define (deriv f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))
```

Now we can do things like this:

```
(define (cube x) (* x x x))
(define dx 0.00001)
((deriv cube) 5)
75.00014999664018
```

#### Abstractions and first-class procedures

- Compound procedures permit us to express general methods of computing as explicit elements in our programming language.
- Higher-order procedures permit us to manipulate these general methods to create further abstractions.
- We should always be on the lookout for underlying abstractions that can be brought out and generalized.
- This doesn't mean we should always program in the most abstract form possible; there is a level appropriate for each task.
- Elements with the fewest restrictions are _first-class_:
	- They may be named by variables.
	- They may be passed as arguments to procedures.
	- They may be returned as the results of procedures.
	- They may be included in data structures.
- In Lisp, procedures have first-class status. This gives us an enormous gain in expressive power.

# Building Abstractions with Data

- Recap: we looked at computations processes and the role of procedures in program design.
	- We saw primitive data (numbers), primitive operations (arithmetic operators), combinations, and abstractions.
	- A procedure is a pattern for the local evolution of a process.
	- We also saw higher-order procedures.
- Now we are going to look at more complex data.
- Before we made compound procedures from other procedures; now we will make compound data from other, simpler data.

> the ability to construct compound data objects enables us to deal with data at a higher conceptual level than that of the primitive data objects of the language. (108)

- Consider functions dealing with rational numbers: this becomes awkward if they can return only one number.
- This is where compound data objects come in handy.
- _Data abstraction_ is separating representation from use.
- Data abstraction enables us to construct _abstraction barriers_ between different parts of the program.
- We will look at closure, conventional interfaces, symbolic expressions, generic operations, and data-directed programming.

## Introduction to data abstraction

- We already know about procedural abstraction: the procedure is a black box, and we don't care how it is implemented internally.
- Data abstractions allows us to isolate how a compound data object is used from the details of its actual representation.

> That is, our programs should use data in such a way as to make no assumptions about the data that are not strictly necessary for performing the task at hand.

- There is a concrete data representation behind the abstraction.
- The interface between the two parts of the system is a set of procedures: selectors and constructors.

### Example: Arithmetic operations for rational numbers

- We want to add, subtract, multiply, divide, and test equality with our rational numbers.
- We assume we have `(make-rat <n> <d>)`, `(number <x>)`, and `(denom <x>)` available as the constructor and selectors.
- This is wishful thinking, and it is a good technique.
- A _pair_ is a concrete structure that we create with cons.
- We extract the parts of the pair with car and cdr.

```
(define x (cons 1 2))
(car x)
1
(cdr x)
2
(define y (cons 3 4))
(define z (cons x y))
(car (car z))
1
(car (cdr z))
3
```

- This is all the glue we need to implement all sorts of complex data structures.
- Data objects constructed from pairs are _list-structured_ data.
- To ensure that our rational numbers are always in lowest terms, we need `make-rat` to divide the numerator and the denominator by their greatest common divisor (GCD).

### Abstractions barriers

> In general, the underlying idea of data abstraction is to identify for each type of data object a basic set of operations in terms of which all manipulations of data objects of that type will be expressed, and then to use only those operations in manipulating the data. (118)

- The details on the other side of an abstraction barrier are irrelevant to the code on this side.
- This makes programs easier to maintain and modify.

> Constraining the dependence on the representation to a few interface procedures helps us design programs as well as modify them, because it allows us to maintain the flexibility to consider alternate implementations. (121)

### What is meant by data?

- Data is defined by some collection of selectors and constructors, together with specified conditions that these procedures must fulfill in order to be a valid representation.
- For rationals, we have the following definition:
1.1. We can construct a rational `x` with `(make-rat n d)`.
1.2. We can select the numerator with `(numer x)`.
1.3. We can select the denominator with `(denom x)`.
1.4. For all values of `x`, `(/ (numer x) (denom x))` must equal n/d.
- For pairs, it is even simpler: we need three operations, which we will call cons, car, and cdr, such that if z is (cons x y), then (car z) is x and (cdr z) is y.
- Any triple of procedures satisfying this definition can be used to implement pairs. In fact, we can do it with procedures themselves and nothing else:

```
(define (cons x y)
  (lambda (m)
    (if (= m 0) x y)))
(define (car z) (z 0))
(define (cdr z) (z 1))
```

- This doesn't look like _data_, but it works.
- This is how you implement pairs in the λ-calculus.
- In real Lisp implementations, pairs are implemented directly, for reasons of efficiency -- but they _could_ be implemented this way and you wouldn't be able to tell the difference.

> the ability to manipulate procedures as objects automatically provides the ability to represent compound data. (125)

- This style of programming is often called _message passing_.

### Extended Exercise: Interval arithmetic

- We want to design a system that allows us to manipulate inexact quantities with known precision (uncertainty).
- To do this, we need arithmetic operations for combining intervals -- ranges of possible values.

## Hierarchical data the closure property

- Paris form a primitive "glue" for compound data objects.
- We can visualize cons pairs with _box-and-pointer_ notation.
- Each pair is a double box. Both the left box and the right box contain an arrow pointing to something else: either to a primitive data object, or to another cons pair.
- The _closure property_ of cons is the ability to make pairs whose elements are pairs.
- Closure allows us to create hierarchal structures.
- We have been using closure all along with combinations. Now, we are going to use closure for compound data.

### Representing sequences

- Among the things we can build with pairs is a sequence.
- A sequence is an ordered collection of data objects.
- The `car` of each pair is the corresponding item in the chain, and the `cdr` of the pair is the next pair in the chain.
- The `cdr` of the final pair is a special value, nil.
- This sequence of nested conses is called a _list_.
- We usually represent such lists by placing each element one after the other and enclosing the whole thing in parentheses.
- The procedure `car` gives us the first item; `cdr` gives us the sublist containing all items but the first; `cons` returns a list with an item added to the front.
- The nil value can be thought of as an empty list.

#### List operations

- We can get at the nth item of the list by `cdr`ing one less than n times, and then taking the `car`.
- Scheme includes a primitive predicate `null?` which is true if its argument is the empty list.

```
(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))
```

- We often write recursive procedures that `cdr` all the way through the list.

```
(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))
```

- We can build up lists to return by `cons`ing them up.

```
(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1 (append (cdr list1) list2))))
```

#### Mapping over lists

- Useful operation: applying the same transformation to each element in a list, producing a new list.
- This is a higher-order procedure called `map`.

```
(define (map f xs)
  (if (null? xs)
    nil
    (cons (f (car xs)
          (map f (cdr xs)))))
```

- Using map, we can do `(map abs (list -1 5 -3 0 2 -2))` and get back the list `(1 5 3 0 2 2)`.
- Map establishes a higher level of abstraction for dealing with lists. It suppressive the recursive detail.
- We think about the process differently when we use map.

> this abstraction gives us the flexibility to change the low-level details of how sequences are implemented, while preserving the conceptual framework of operations that transform sequences to sequences. (144)

### Hierarchical structures

- We can represent lists whose elements themselves may also be lists. We can also think of them as _trees_.
- Recursion is a natural tool for dealing with trees.
- The primitive predicate `pair?` returns true if its argument is a pair (formed with cons).
- We can count the number of leaves in a tree with this:

```
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
```

#### Mapping over trees

- We can deal with trees using `map` together with recursion.
- This makes it possible to apply an operation to all the leaves in a tree, for example.

### Sequences as conventional interfaces

- Abstractions preserves the flexibility to experiment with alternative representations.
- Another powerful design principle for working with data structures is the use of conventional interfaces.
- To make abstract operations for things other than numbers, we need to have a conventional style in which we manipulate data.

#### Sequence operations

- Key to organizing programs to reflect signal-flow structure: focus on the signals and represent them as lists.
- To help, we can implement `filter` and `accumulate`.
- Expressing programs as sequence operations helps us make program designs that are _modular_ -- made of relatively independent pieces that we can connect in flexible ways.
- This is a strategy for controlling complexity.
- A vast range of operations can be expressed as sequence operations, even if you don't realize it at first.
- Sequences (here, they are lists) serve as a conventional interface for the modules of the program.

#### Nested Mappings

- For many computations, the sequence paradigm can be used where loops would otherwise be needed.
- Sometimes we need to use _nested_ mappings, where each mapping maps to a second set of mappings.
- We can use `mapcat` to flatten the nested mapping result into one list at the end.
- The procedure for computing all permutations of a set is pure magic: this is wishful thinking in action!

```
(define (permutations s)
  (if (null? s)
    (list nil)
    (mapcat (lambda (x)
              (map (lambda (p) (cons x p))
                   (permutations (remove x s))))
            s)))
```

### Example: A picture language

- We will use a simple language for drawing pictures.
- The data objects are represented as procedures rather than as list structure.

#### The picture language

- There is only one kind of element, called a _painter_.
- The painter draws an image transformed into a parallelogram.
- We combine images with operations like `beside` and `below`.
- We transform single images with operations like `flip-vert` and `flip-horiz`.
- We can build up complexity easily because painters are closed under the language's means of combination (this is closure).

#### Higher-order operations

- Just as we have higher-order procedures, we can have higher-order painting operations.
- We can manipulate the painter operations rather than manipulating the painters directly.

#### Frames

- Painters paint their contents in frames.
- A frame can be represented by three vectors: a position vector for one of the corners, and two vectors going along the edges.
- We can use data abstraction to avoid being specific about how these vectors will actually be represented.
- We will use coordinates in the unit square.
- We can use basic vector operations to map an image coordinate into a pair of coordinates within the frame.

#### Painters

- A painter is a procedure that takes a frame as an argument and draws its image transformed to fit in the frame.
- The details of primitive painters depend on the characteristics of the graphics system.
- Painters as procedures is a powerful abstraction barrier.

#### Transforming and combining painters

- Operations on painters invoke the original painters with new frames drives from the argument frame.
- They are all based on the procedure `transform-painter`.

#### Levels of language for robust design

- The picture language uses abstraction with procedures and data.
- Painters are the data abstraction.
- This example also uses _stratified design_, the notion that a complex system should be structured as a sequence of levels.
- Each time we start a new level, we treat the old complex things as primitive black boxes and combine them.
- Stratified design helps make programs _robust_: small changes in a specification will likely mean small changes in the program.

> In general, each level of a stratified design provides a different vocabulary for expressing the characteristics of the system, and a different kind of ability to change it.

## Symbolic data

- So far we have constructed compound data from numbers only.
- Now, we will work with arbitrary symbols.

### Quotation

- Lists containing symbols look just like expressions (code).
- We need to _quote_ data objects to manipulate symbols.
- To make the list `(a b)`, we can't just do `(list a b)` because this will evaluate `a` and `b`.
- This is just like natural language. If I say, "Say your name," you will say the _value_ of "your name." If I instead say, "Say 'your name,'" you will literally say the words "your name."
- To quote in Lisp, we place a single quotation mark at the beginning of the object to be quoted.
- Here is the difference between symbols and their values:

```
(define a 1)
(define b 2)
(list a b)   ; => (1 2)
(list 'a 'b) ; => (a b)
(list 'a b)  ; => (a 2)
```

- We can write lists directly with quotation rather than using `cons` or list, and we can represent the empty list with `'()`.
- We need one more primitive now: `eq?`. This tests to see if two symbols are the same.

### Example: Symbolic differentiation

- Consider a procedure that performs symbolic differentiation of algebraic expressions. We need symbols for this.
- We will worry about representation later (data abstraction).

#### The differentiation program with abstract data

- To start, we will only consider addition and multiplication.
- We need the constant rule, the sum rule, and the product rule.
- We will assume we already have these procedures:

```
(variable? e)
(same-variable? v1 v2)
(sum? e)
(addend e)
(augend e)
(make-sum a1 a2)
(product? e)
(multiplier e)
(multiplicand e)
(make-product m1 m2)
```

#### Representing algebraic expressions

- There are many ways we could represent algebraic expressions.
- The most straightforward is in parenthesized Polish notation: Lisp syntax.
- We can simplify the answers just like we did in the rational number example: by changing the constructors.
- Simplifying the way a human would is hard, partly because the most "simplified" form is sometimes subjective.

### Example: Representing sets

There are a number of possible ways we could represent sets. A set is a collection of distinct objects. Our sets need to work with the following operations:

- `union-set`: new set containing all items of given sets
- `intersection-set`: new set containing only the items that all given sets share
- `element-of-set?`: predicate for an item's presence in a set
- `adjoin-set`: new set containing one new item

#### Sets as unordered lists

- One method is to just use lists. Each time we add a new element, we have to check to make sure it isn't already in the set.
- This isn't very efficient.
- The time complexity of `element-of-set?` (and therefore for `adjoin-set`) is $Θ(n)$.
- For union and intersection, it's $Θ(n^2)$.
- If we don't eliminate duplicates, most operations are faster.
- On the other hand, the sets grow in size rapidly, which may cause an overall decrease in performance.

#### Sets as ordered lists

- A more efficient representation is a sorted list.
- We will only consider sets of numbers to keep things simple.
- Now, scanning the entire list in `element-of-set?` is a worst-case scenario. Most of the time we won't have to.
- On average we should expect to scan about half of the set.
- This is still $Θ(n)$; however, intersection gets much faster. Instead of $Θ(n^2)$, it is $Θ(n)$.

#### Sets as binary trees

- An even more efficient representation: binary trees.
- Each node contains an element of the set.
- The left branch contains smaller numbers, and the right branch contains larger numbers.
- The same set can be represented by a tree in many ways.
- If the tree is balanced, each subtree is about half the size of the original tree.
- This allows us to do `element-of-set?` in $Θ(log(n))$ time.
- We will use the following list structure to represent trees: each node is the list `(number left-subtree right-subtree)`.
- Efficiency hinges on the tree being balanced.
- We can write a procedure to balance trees, or we could use a difference data structure (B-trees or red-black trees).

#### Sets and information retrieval

- The techniques discussed for sets show up again and again in information retrieval.
- In a data-management system, each record is identified by a key.
- The key must be unique.
- The simplest, least efficient method is to use a set of records represented by an unordered list. This provides $Θ(n)$ access.
- For "random access," meaning $Θ(1)$ access time complexity, trees are usually used.
- Data abstraction is important here -- you could begin by using unordered lists, and then change the constructor and selectors to use a tree representation.

### Example: Huffman encoding trees

- We can represent data as sequences of ones and zeros (bits).
- Codes like ASCII are fixed-length. Each symbol is represented by the same number of bits.
- On the other hand, Morse code is variable-length. Since _e_ is the most common letter, it is represented by a single dot.
- The issue with variable-length is that you must have a way of knowing when you've reached the end of a symbol.
- Morse code uses a temporal pause. Prefix codes, like Huffman encoding, ensure that no code for a symbol is a prefix of the code for another symbol. This eliminates any ambiguity.
- A Huffman code can be represented as a binary tree with a symbol and its weight (relative frequency) at each leaf.
- Each non-leaf node has a weight (sum of all below) and the set of all the symbols below it. The root node has the set of all the symbols in the tree.
- Each left branch is a zero, and each right branch is a one.

#### Generating Huffman trees

- Huffman gave an algorithm for constructing the best code for a given set of symbols and their relative frequencies.
- Begin with all the nodes as leaves, then form a node branching off to the two least frequent symbols.
- Repeat until there is only one node left, the root node.

#### Representing Huffman trees

- Leaves are represented by the list beginning with the symbol `leaf` and with two more elements: the symbol and the weight.
- Trees are represented by the list `(left right symbols weight)`, where `left` and `right` are subtrees, `symbols` is a list of the symbols underneath the node, and `weight` is the sum of the weights of all the leaves beneath the node.

## Multiple representations for abstract data

- Data abstraction lets use write specify programs that work independently of the chosen representation for data objects.
- We erect abstraction barriers to control complexity.
- This still isn't powerful enough -- it doesn't always make sense to speak of the "underlying representation."
- We might want to deal with multiple representations.
- For example, complex numbers can be represented in rectangular form or in polar form.
- We need abstraction barriers that isolate representation from use _and_ others that isolate design choices.
- We want to permit multiple design choices to coexist.
- This is important for modularity.
- We will construct _generic procedures_, which can operate on data that may be represented in more than one way.
- We will accomplish this with _type tags_. We will also discuss _data-directed_ programming.
- We now have horizontal abstraction barriers, separating higher-level from lower-level, and vertical ones, separately alternative representations.

### Representations for complex numbers

- This system will perform arithmetic operations on complex numbers represented in rectangular form _or_ polar form.
- Different representations are appropriate for different operations.
- We have four selectors: `real-part`, `imag-part`, `magnitude`, and `angle`.
- We have two constructors: `make-from-real-img` and `make-from-mag-ang`.

### Tagged data

- One way to view data abstraction: principle of least commitment.
- We waited until the last minute to choose the concrete representation, retaining maximum flexibility.
- We can take it even further: let's use _both_!
- To include both in the same system, we need some way of distinguishing between them.
- We will use a symbol as a type tag.
- Each generic selector uses case analysis to check the tag of its argument and dispatches the appropriate procedure.
- Our general mechanism for interfacing separate representations: in a specific implementation, the data object is an untyped pair. The generic selectors dispatch on the tag and strip off the tag before passing the data to the appropriate procedure.

### Data-directed programming and additivity

- The strategy of checking the type and calling an appropriate procedure is called dispatching on type.
- The implementation in the previous section had two significant weaknesses:
1.1. The generic interface procedures must know about all the different representations. Adding a new representation means adding a clause to all the generic procedures.
1.2. We must guarantee that no two procedures have the same name.
- The underlying issue: the technique we used was not _additive_.
- This is a source of inconvenience and error.
- The solution is a technique known as data-directed programming.
- We imagine a table with operations on one axis and possible types on the other axis.
- Before, we used case analysis to perform explicit dispatch.
- With DDP, we use a single procedure that looks up the operation name and the argument type in the table.
- We assume we have procedures `put` and `get`.
- `(put op type item)` installs `item` in the table.
- `(get op type)` retrieves the item.

#### Message passing

- Data-directed programming handles generic operations by dealing explicitly with operation-and-type tables.
- In our example, each operation took care of its own dispatching.
- An alternative approach is to dispatch on the operator name within the data object itself.
- Rather than applying a generic operation to a data object on which it dispatches to the appropriate procedure, we apply the data object to a message.
- This is called message-passing style, and we can accomplish it in Scheme using closures.
- Message passing can be a powerful tool for structuring simulation programs.

## Systems with generic operations

- The key idea in the previous section: link specific data operations to multiple representations using generic procedures.
- We can extend this further to create operations that are generic over different kinds of arguments, not just different representations of the same kind of data.
- We have seen several different arithmetic packages -- primitive numbers, rational numbers, intervals, and complex numbers.
- We will use data-directed techniques to write procedures that work on all of these data structures.
- This will require many abstraction barriers.
- The result will be additive (modular) -- easy to add new types.

### Generic arithmetic functions

- We want `add` to work for primitive numbers, rational numbers, and complex numbers.
- We will attach a type tag to each kind of number.
- Just as we did in our first data-directed example, we will install a "package" for each kind of data.
- For complex numbers we have two levels of tagging: a `'complex` tag on top of the `'rectangular` or `'polar` tag.
- The tags get stripped off as the data is passed down through packages to the appropriate specific procedure.

### Combining data of different types

- In our unified arithmetic system, we ignored an important issue.
- We did not consider the possibility of operations that cross type boundaries, like adding a scheme-number to a rational.
- One solution would be to design a procedure for each possible combination of types.
- This is cumbersome, and it violates modularity.

#### Coercion

- Often the different data types are not completely independent.
- For example, any real number can be expressed as a complex number with an imaginary part of zero (but not vice versa).
- We can make all operations work on combinations of schem-numbers and complex numbers by promoting, or coercing, the former to the latter type and then using the complex number procedure.
- Here is a typical coercion procedure:

```
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
```

- In addition to the generic procedure table, we would need a special coercion table.
- We can modify `apply-generic` to try coercion if there is no specific procedure available for the given types.
- The big advantage of coercion is that, although we may need to write many coercion procedures, we only need to write specific operation procedures once per type.
- It gets more complicated: what if both types can be converted to a third type? What if A can be converted to B and then from B to C? We have a graph of relations among the types.

#### Hierarchies of types

- Integers are a subtype of rationals, which are a subtype of real numbers, and so on for complex numbers.
- We have supertypes going in the opposite order.
- A simple hierarchy where each type has at most one supertype and and most one subtype is called a tower.
- Coercion then simply becomes a matter of raising the argument whose type is lower in the tower to the level of the other type.
- We can write fewer procedures by allowing types to _inherit_ the operations on supertypes.
- In some cases we can also lower a value down the type tower.

#### Inadequacies of hierarchies

- In general, a type may have more than one subtype (not a tower).
- Having multiple super types is tricky, since the type can be raised via multiple paths to search for a procedure.
- Large numbers of interrelated types conflicts with modularity.

### Example: Symbolic algebra

- The manipulation of symbolic algebraic expressions is hard.
- We can view them as hierarchical structures -- a tree of operators applied to operands.
- A complete system would be exceedingly complex.
- We will just look at the arithmetic of polynomials.

#### Arithmetic on polynomials

- A polynomial is relative to certain variables, the _indeterminates_.
- We will only consider univariate polynomials.
- The polynomial is a sum of terms, each being a coefficient, a power of the indeterminate, or a product of the two.
- A coefficient is an algebraic expression that is not dependent upon the indeterminate of the polynomial.
- Our polynomials will be syntactic forms, not representations of mathematical functions.
- This means that replacing each $x$ with $y$ creates a different polynomial, although they could represent the same function.
- We will only do addition and multiplication.
- Both operands must have the same indeterminate.
- The _poly_ data structure consists of a variable (the indeterminate) and a list of terms.
- Our system works with polynomials with polynomial coefficients "for free" because of data-directed recursion -- because we are using generic procedures.

#### Representing term lists

- Our procedures `add-terms` and `mul-terms` always access term lists sequentially from highest to lowest order.
- So, we need some kind of ordered list representation.
- We can choose between _dense_ and _sparse_ representations.
- Dense term lists, where most coefficients are nonzero, are best represented by simple lists.
- Sparse term lists, where there are many zeros, are better represented by some kind of associative list or map.

#### Hierarchies of types in symbolic algebra

- Our polynomial system used complex objects that had objects of many different parts as types.
- It was recursive data abstraction because the terms of the polynomial could themselves be polynomials.
- The data types in polynomial algebra cannot be arranged in a tower.
- Controlling coercion is a hard problem in these systems.

#### Extended exercise: Rational functions

- Rational functions are "fractions" with polynomial numerators and denominators.
- We can use our old rational package, but we need to change a few things (like using generic `add`, `mil`, etc).
- We can reduce rational functions as long as we can compute the GCD of two polynomials (which in turn uses the remainder operation on two polynomials).
- Problem: we end up with a GCD polynomial that has fractional coefficients. Solution: multiply the first argument by an _integerizing factor_.
- We can use our GCD procedure to reduce rational functions to lowest terms:
1.1. Compute the integerized GCD of the numerator and denominator.
1.2. Multiply the numerator and denominator by an integerizing factor: the leading coefficient of the GCD raised to the power $1 + O_1 − O_2$, where $O_2$ is the order of the GCD and $O_1$ is the maximum of order of the numerator and the order of the denominator.
1.3. Divide the new numerator and new denominator by the GCD.
1.4. Divide both by the GCD of all their coefficients.
- This GCD algorithm, or something like it, is at the heart of every system that does operations on rational functions.
- This one is very slow. Probabilistic algorithms are faster.

# Modularity, Objects, and State

- Primitive procedures and primitive data combine to construct compound entities; abstraction controls complexity.
- But these tools are not sufficient for designing programs.
- We also need organizational principles to guide the overall design of the program.
- We need to structure large systems to make them _modular_.
- A modular program can be divided into coherent parts that can be separately developed and maintained.
- One designed strategy is to base the structure of the program on the structure of physical systems being modelled.
- Done properly, this allows us to add new objects or new actions easily and locally, without changing the whole strategy.
- Program organization depends on the system to be modelled.
- Two "world views": concentrate on _objects_, or on _streams_.
- Objects: Must allow change but preserve identity. Abandon the substation model for the environment model. Grapple with time in the computational model.
- Streams: Decouple simulated time in the model from the order of events that take place in the computer. Delayed evaluation.

## Assignment and local state

- The world is populated by independent objects possessing changing state.
- An object "has state": its behaviour is influenced by history.
- A bank account is an example of a stateful object.
- An object's state can be characterized by _state variables_.
- We need an _assignment operator_ to change the value associated with a name representing a local variable of an object.

### Local state variables

- Let's model the situation of withdrawing from a bank account.
- The `withdraw` procedure should accept an amount of money as an argument and return the balance after the withdrawal.
- If you try to withdraw too much, it should return the string "Insufficient funds".
- Suppose we begin with $100:

```
(withdraw 25)
75
(withdraw 25)
50
(withdraw 60)
"Insufficient funds"
(withdraw 15)
35
```

- Evaluating the same combination twice, `(withdraw 25)`, returned different values.
- We have lost referential transparency. This is a new kind of behaviour of a procedure. Until now, the returned value depended only on the arguments, like a mathematical function.
- To implement `withdraw`, we define a variable called `balance`:

```
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds"))
```

- This uses the `set!` special form, whose syntax is `(set! <name> <new-value>)`.
- Procedures names that end with a bang change the values of variables or of data structures.
- The expression `(begin <exp1> <exp2> ... <expk>)` evaluates all the expressions in sequence and returns the value of the last.
- We made `balance` a global variable. It is much better to have it _encapsulated_ within `withdraw`, like so:

```
(define withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))
```

- Unfortunately, the substitution model of evaluation is no longer adequate once we have assignment in our procedures.
- For now, we technically have no way to understand how these procedures work. We will develop a new model soon.
- The following procedure creates "withdraw processors":

```
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds")))
(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(W1 50)
50
(W2 70)
30
(W2 40)
"Insufficient funds"
(W1 40)
10
```

- Here, `W1` and `W2` are complement independent objects, each with its own local state variable.
- We can create a "bank-account object" that responds to multiple messages, all operating on the same local state:

```
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)
```

### The benefits of introducing assignment

- Introducing assignment "leads us into a thicket of difficult conceptual issues" (305).
- Even so, it gives us a powerful technique for maintaining modular design.
- Consider the procedure `rand`. We want it to return an integer chosen at random each time we evaluate `(rand)`.
- Suppose we have the procedure `rand-update` that takes one argument.
	- We first call it with an initial value $x_1$, and it returns an integer $x_2$.
	- We call it with $x_2$, and it gives us $x_3$, and so on.
	- The sequence of values $x_1, x_2, ..., x_n$ will have the desired statistical properties (random uniform distribution).
	- This is a _pseudorandom_ sequence, since each number is a function of the previous one.
- We could implement `rand` like this:

```
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
```

> The Monte Carlo method consists of choosing sample experiments at random from a large set and then making deductions on the basis of the probabilities estimated from tabulating the results of those experiments. (306--307)

- We can use the Monte Carlo method to approximate $π$, knowing that the probability that two randomly chosen integers have 1 as their GCD is $6//π^2$.
- If we had to use `rand-update` directly, our Monte Carlo program would "betray some painful breaches of modularity" (308).

> The general phenomenon illustrated by the Monte Carlo example is this: From the point of view of one part of a complex process, the other parts appear to change with time. They have hidden time-varying local state. If we wish to write computer programs whose structure reflects this decomposition, we make computational objects (such as bank accounts and random-number generators) whose behaviour changes with time. We model state with local state variables, and we model the changes of state with assignments to those variables. (309)

### The costs of introducing assignment

- The advantages of local state and assignment come at a price.
- The substitution model of procedure application is no longer sufficient to properly interpret out programs.
- Programming without the use of assignment, as we have done in the first two chapters, is called _functional programming_.
- Observe what happens when we try using the substation model:

```
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
((make-simplified-withdraw 25) 20)
((lambda (amount) (set! balance (- 25 amount)) 25) 20)
(set! balance (- 25 20)) 25
```

- Adhering to the substation model, we set `balance` to 5 and then return 25 as the value of the expression.
- This is wrong. We shouldn't have substituted 25 for `balance` everywhere, because the assignment changed it.
- Before, a variable was simply a name for a value.
- Now, since a variable can change, it somehow refers to a place where a value can be stored, and this value can be changed.

#### Sameness and change

- By introducing change into our computational models, many previously straightforward notions become problematic.
- Consider the concept of two things being "the same."
- If we have `(make-withdraw 25)` and `(make-withdraw 25)`, are they the same? No, because they can have different local state.
- A language that supports "equals can be substituted for equals" is _referentially transparent_. `set!` violates this.
- Reasoning about programs that use assignment is much more difficult for this reason.
- The concepts of "sameness" and "change" chase each other around in circles; it is hard to formally define them.
- If we have `(define peter-acc (make-account 100))`, there is a big difference between defining `paul-acc` in the same way and defining it with `(define paul-acc peter-acc)`.
- In the first case, they have distinct accounts. In the second, both refer to the same account -- this is called _aliasing_.
- As long as we never modify objects, we can regard them to be precisely the totality of their pieces.
- This is no longer valid in the presence of change, because "identity" is something different from the pieces.
- A bank account is still "the same" account after a withdrawal. Conversely, two distinct accounts with the same balance are "different."
- The name is attached to an identity rather than the data itself.

#### Pitfalls of imperative programming

- Programming that makes extensive use of assignment is called _imperative programming_.
- Imperative programs are susceptible to bugs that cannot occur in functional programs.
- Things get even worse when we throw concurrency into the mix.

> In general, programming with assignment forces us to carefully consider the relative orders of the assignments to make sure that each statement is using the correct version of the variables that have been changed. This issue simply does not arise in functional programs. (318--319)

## The environment model of evaluation

- Recall the substitution model:

> To apply a compound procedure to arguments, evaluate the body of the procedure with each formal parameter replaced by the corresponding argument.

- This is no longer adequate once we allow assignment.
- Variables are no longer merely names for values; rather, a variable designates a "place" in which values can be stored.
- These places will be maintained in structures called _environments_.
- An environment is a sequence of _frames_. A frame is a table of _bindings_. A binding associates a variable name with one value.
- Each frame also has a pointer to its enclosing environment, unless it is considered to be global.
- The value of a variable with respect to an environment is the value given by the binding in the first frame of the environment that contains a binding for that variable.
- If no frame in the sequence specifies a binding for the variable, then the variable is _unbound_ in the environment.
- A binding _shadows_ another (of the same variable) if the other is in a frame that is further in the sequence.
- The environment determines the context in which an expression should be evaluated. An expression has no meaning otherwise.
- The global environment consists of a single frame, and it is implicitly used in interactions with the interpreter.

### The rules for evaluation

- To evaluate a combination:
1.1. Evaluate the subexpressions of the combination.
1.2. Apply the value of the operator subexpression to the values of the operand subexpressions.
- The environment model redefines the meaning of "apply."
- A procedure is created by evaluating a λ-expression relative to a given environment.
- The resulting procedure object is a pair consisting of the text of the λ-expression and a pointer to the environment in which the procedure was created.
- To apply a procedure to arguments, create a new environment whose frame binds the parameters to the values of the arguments and whose enclosing environment is specified by the procedure.
- Then, within the new environment, evaluate the procedure body.
- `(lambda (x) (* x x))` evaluates a to pair: the parameters and the procedure body as one item, and a pointer to the global environment as the other.
- `(define square (lambda (x) (* x x)))` associates the symbol `square` with that procedure object in the global frame.
- Evaluating `(define <var> <val>)` creates a binding in the current environment frame to associate `<var>` with `<val>`.
- Evaluating `(set! <var> <val>)` locates the binding of `<var>` in the current environment (the first frame that has a binding for it) and changes the bound value to `<val>`.
- We use `define` for variables that are currently unbound, and `set!` for variables that are already bound.

### Applying simple procedures

- Let's evaluate `(f 5)`, given the following procedures:

```
(define (square x) (* x x))
(define (sum-of-square x y) (+ (square x) (square y)))
(define (f a) (sum-of-squares (+ a 1) (* a 2)))
```

- These definitions create bindings for `square`, `sum-of-squares`, and `f` in the global frame.
- To evaluate `(f 5)`, we create an environment $E_1$ with a frame containing a single binding, associating `a` with `5`.
- In $E_1$, we evaluate `(sum-of-squares (+ a 1) (* a 2))`.
- We must evaluate the subexpressions of this combination.
- We find the value associated with `sum-of-squares` not in $E_1$ but in the global environment.
- Evaluating the operand subexpressions yields `6` and `10`.
- Now we create $E_2$ with a frame containing two bindings: `x` is bound to `6`, and `y` is bound to `10`.
- In $E_2$, we evaluate `(+ (square x) (square y))`.
- The process continues recursively. We end up with `(+ 36 100)`, which evaluates to `136`.

### Frames as the repository of local state

- Now we can see how the environment model makes sense of assignment and local state.
- Consider the "withdrawal processor":

```
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds")))
```

- This places a single binding in the global environment frame.
- Consider now `(define W1 (make-withdraw 100))`.
	- We set up $E_1$ where `100` is bound to the formal parameter `balance`, and then we evaluate the body of `make-withdraw`.
	- This returns a lambda procedure who environment is $E_1$, and this is then bound to `W1` in the global frame.
- Now, we apply this procedure: `(W1 50)`.
	- We construct a frame in $E_2$ that binds `amount` to `50`, and then we evaluate the body of `W1`.
	- The enclosing environment of $E_2$ is $E_1$, _not_ the global environment.
	- Evaluating the body results in the `set!` rebinding `balance` in $E_`$ to the value `(- 100 50)`, which is `50`.
	- After calling `W1`, the environment $E_2$ is irrelevant because nothing points to it.
	- Each call to `W1` creates a new environment to hold `amount`, but uses the same $E_1$ (which holds `balance`).
- `(define W2 (make-withdraw 100))` creates another environment with a `balance` binding.
	- This is independent from $E_1$, which is why the `W2` object and its local state is independent from `W1`.
	- On the other hand, `W1` and `W2` share the same code.

### Internal definitions

- With block structure, we nested definitions using `define` to avoid exposing helper procedures.
- Internal definitions work according to the environmental model.
- When we apply a procedure that has internal definitions, there are `define` forms at the beginning of the body.
- We are in $E_1$, so evaluating these adds bindings to the first frame of $E_1$, right after the arguments.
- When we apply the internal procedures, the formal parameter environment $E_n$ is created, and its enclosing environment is $E_1$ because that was where the procedure was defined.
- This means each internal procedure has access to the arguments of the procedure they are defined within.
- The names of local procedures don't interfere with names external to the enclosing procedure, due to $E_1$.

## Modeling with mutable data

- We previously looked at compound data and data abstraction.
- To model stateful objects, we need _mutators_ in addition to constructors and selectors.
- A mutator is a procedure that modifies the data object.

### Mutable list structure

- The primitive mutators for pairs are `set-car!` and `set-cdr!`.
- `(set-car p x)` changes the `car` of the pair `p`, making it point to `x` instead.
- The old `car` is unreachable garbage. We will see later how Lisp recycles this memory.
- We could implement `cons` in terms of these two procedures in addition to a `get-new-part` procedure.

```
(define (cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))
```

#### Sharing and identity

- Consider `(define x (list 'a 'b))` and `(define z1 (cons x x))`.
- `z1` is a pair whose `car` and `cdr` both point to the same `x`.
- In contrast: `(define z2 (cons (list 'a 'b)) (list 'a 'b))`.
- In `z2`, the two `(a b)` lists are distinct, although the actual symbols are shared.
- Before assignment, we would think `z1` and `z2` were "the same."
- The sharing is undetectable without mutators on list structure.
- It _is_ detectable in our environmental model.
- If we `set-car!` on the `car`, this will change both `a` symbols in `z1` but only the first in `z2`.
- We can use the predicate `eq?` to test for sameness in the sense of identity.
- `(eq? x y)` tests whether `x` and `y` point to the same object.
- We can exploit sharing for good, but it can be dangerous.

#### Mutation is just assignment

- Earlier we said we can represent pairs purely in terms of procedures:

```
(define (cons x y)
  (lambda (sel)
    (sel x y)))
(define (car p)
  (p (lambda (x y) x)))
(define (cdr p)
  (p (lambda (x y) y)))
```

- The same is true of mutable data. We can implement mutators with procedures and assignment alone:

```
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation: CONS" m))))
  dispatch)
```

- Assignment and mutation are equipotent: each can be implemented in terms of the other.

### Representing queues

- The mutators allow us to construct new data structures.
- A _queue_ is a sequence in which items are inserted at one end (the rear) and deleted from the other end (the front).
- It is also called a FIFO buffer (first in, first out).
- We define the following operations for data abstraction:
	- a constructor `(make-queue)`,
	- a predicate `(empty-queue? q)`,
	- a selector `(front-queue q)`,
	- two mutators: `(insert-queue! q x)` and `(delete-queue! q)`.
- A simple list representation is inefficient because we have to scan to get to one end.
- Scanning a list takes $Θ(n)$ operations.
- A simply modification lets us implement all the operations with $Θ(1)$ time complexity: keep a pointer to the end as well.
- A queue is a pair formed by consing the front-pointer and the rear-pointer of a normal list.

### Representing tables

- In a one-dimensional table, each value is indexed by one key.
- We can implement it as a simple list of _records_.
- A record is a pair consisting of a key and an associated value.
- The first record in the table is a dummy, and it hold the arbitrarily chosen symbol `*table*`.
	- If a table was just a pointer to the first actual record, then when we wouldn't be able to write a mutator to add a record to the front.
	- We would need to change the table to point to the new front, but  `set!` on a formal parameter doesn't work as desired.
	- It would only change the parameter in $E_1$, not the value in the calling environment.
	- We didn't need to worry about this with sets because a set was a cons of two pointers a therefore we could mutate the `car` and `cdr` -- but we couldn't change the set _itself_, since it was effectively a pointer to the pair, _copied_ on application.
	- We are essentially using a pointer; we are using one cell of the cons pair. Some schemes provide `box`, `unbox`, and `set-box!` for this purpose. In C, these are `&x`, `*x`, and `*x = ...`.
- The `lookup` procedure returns the value associated with a key in a table, or `#f` if it cannot be found.
- It uses `assoc`, which returns the whole record rather than just the associated value.

#### Two-dimensional tables

- Two-dimensional tables are indexed by two keys.
- In some cases, we could just use a one dimensional table whose keys are pairs of keys.
- We can implement a two-dimensional table as a one-dimensional table whose values are themselves one-dimensional tables.
- We could just use them like that without any specific procedures. However, insertion with two keys is complex enough to merit a convenient two-dimensional procedure.
- We don't need to box the subtables. They key of the record serves the purpose of `'*table*`.

#### Creating local tables

- With our `lookup` and `insert!` procedures taking the table as an argument, we can manage multiple tables.
- Another approach is to have separate procedures for each table.
- We could use the message-passing style with the `dispatch` procedure that we've seen a few times already.
- We could also take the λ-calculus approach:

```
(define (make-table) (lambda (k) #f))
(define (lookup key table) (table key))
(define (insert! key value table)
  (lambda (k)
    (if (eq? k key)
      value
      (table k))))
```

### A simulator for digital circuits

- Digital circuits are made up of simple elements.
- Networks of these simple elements can have very complex behaviour.
- We will design a system to simulator digital logic. This type of program is called an _event-driven simulation_.
- Our computational model is based on the physical components.
	- A _wire_ carries a _digital signal_, which is either 0 or 1.
	- A _function box_ connects to input wires and output wires.
	- The output signal is delayed by a time that depends on the type of function box.
	- Our primitive function boxes are the _inverter_, _and-gate_, and _or-gate_. Each has its own delay.
	- We construct complex functions by connecting primitives.
	- Multiple outputs are not necessarily generated at the same time.
- We construct wires with `(make-wire)`.
- Evaluating `(define a (make-wire))` and `(define b (make-wire))` followed by `(inverter a b)` connects `a` and `b` with an inverter.
- The primitive functions are our primitive elements; wiring is the means of combination; specifying wiring patterns as procedures is the means of abstraction.

#### Primitive function boxes

- The primitives boxes implement "forces" by which changes in the signal of one wire influence the signal of another.
- We have the following operations on wires:
	- `(get-signal <wire>)` returns the current value of the signal.
	- `(set-signal! <wire> <value>)` changes the value of the signal.
	- `(add-action! <wire> <procedure of no arguments>)` asserts that the given procedure should be run whenever the signal on the wire changes value.
- The procedure `after-delay` executes a procedure after a given time delay.

#### Representing wires

- Our wires will be computational objects each having two local state variables: `signal-value` and `action-procedures`.
- We use the message-passing style as before.
- Wires have time-varying signals and can be incrementally attached to devices. They are a good example of when you need to use a mutable object in the computational model.
- A wire is shared between the devices connected to it. If one changes it, the rest see the change.
- This would be impossible if you didn't model the wire as an identity, separate from its signal value.

#### The agenda

- The only thing left is `after-delay`.
- An _agenda_ is a data structure that schedules things to do.
- For an agenda `(define a (make-agenda))`, the operations `(empty-agenda? a)`, `(first-agenda-item a)`, `(remove-first-agenda-item! a)`, and `(current-time a)` are self-explanatory.
- We schedule new items with `(add-to-agenda <time> <action> a)`.
- We call the global agenda `the-agenda`.
- The simulation is driven by `propagate`, which executes each item on the agenda in sequence.

#### Implementing the agenda

- The agenda is a a pair: the current time, and a list of _time segments_ sorted in increasing order of time.
- A time segment is a pair: a number (the time) and a queue of procedures that are scheduled to run during that time segment.
- To add an action to the agenda, we scan its segments, examining their times. If we find the right time, we add the action to that segment's queue. Otherwise, we create a new segment.
- To remove the first agenda item, we delete the first item in the first queue, and if this makes the queue empty, we delete the first time segment as well.
- Whenever we extract the first item with `first-agenda-item`, we also update the current time.

### Propagation of constraints

- We often organize computer programs in one direction: from input to output.
- On the other hand, we often model systems in terms of relations among quantities.
- The equation $dAE = FL$ is not one-directional.
- We will create a language to work in terms of the relations themselves, so that we don't have to write five procedures.
- The primitive elements are _primitive constraints_.
	- `(adder a b c)` specifies $a + b = c$.
	- `(multiplier x y z)` specifies $xy = z$.
	- `(constant 3.14 x)` says that the value of `x` must be 3.14.
- The means of combination are constructing _constraint networks_ in which constraints are joined by _connnectors_.
- The means of abstraction are procedures.

#### Using the constraint system

- We create connectors with `(make-connector)`, just like wires.
- We use `(probe "name" <connector>)`, again just like wires.
- `(set-value! <connector> <value> 'user) assigns a value to the connector, and this information propagates through the network.
- This will give an error if the new value causes a contradiction.
- `(forget-value! <connector> 'user)` undoes the assignment.

#### Implementing the constraint system

- The overall system is simpler than the digital circuit system because there are no propagation delays.
- There are five basic operations on a connector `c`: `(has-value? c)`, `(get-value c)`, `(set-value! c <value> <informant>)`, `(forget-value! c <retractor>)`, and `(connect c <constraint>)`.
- The procedures `inform-about-value` and `inform-about-no-value` tells the constraint that a connector has (lost) a value.
- Whenever an adder gets a new value, it checks if it has two and can calculate the third.

#### Representing connectors

- A connector is a procedural object with local state variables -- again, just like a wire.
- Each time the connector's value is set, it remembers the informant. This could be a constraint, or a symbol like `'user`.
- `for-each-except` is used to notify all _other_ constraints.

## Concurrency: time is of the essence

- The power of stateful computational objects comes at a price: the loss of referential transparency.
- This gives rise to a thicket of questions about sameness and change, and we had to create a more intricate evaluation model.
- The central issue is that by introducing assignment we are forced to admit _time_ in the computational model.
- We can go further in structuring the model to match the world: in the physical world, we perceive simultaneous changes.
- We want computational processes executing _concurrently_.
- Writing programs this way forces us to avoid inessential timing constraints, making the program more modular.
- It can also provide a speed advantage on multicore computers.
- The complexities introduces by assignment become even more problematic in the presence of concurrency.

### The nature of time in concurrent systems

- On the surface, time is straightforward: it is an ordering.
- Two events either occur in one order, or the other, or simultaneously.
- Consider `(set! balance (- balance amount))`. There are three steps: accessing the value of `balance`, computing the new balance, and setting `balance` to this value.
- Two such expressions executed concurrently on the same `balance` variable could have their three steps interleaved.
- The general problem is that, when concurrent processes share a state variable, they may try to change it at the same time.

#### Correct behaviour of concurrent programs

- We already know we have to be careful about order with `set!`.
- With concurrent programs we must be especially careful.
- A very stringent restriction to ensure correctness: disallow changing more than one state variable at a time.
- A less stringent restriction: to ensure that the system produces the same result as if the processes had run sequentially in some order (we don't specify a particular order).
- Concurrent programs are inherently _nondeterministic_, because we don't what order of execution its result is equivalent to, so there is a set of possible values it could take.

### Mechanisms for controlling concurrency

- If one process has three ordered events $(a,b,c)$ and another, running concurrently, has three ordered events $(x,y,z)$, then there are twenty ways of interleaving them.
- The programmer would have to consider the results in all twenty cases to be confident in the program.
- A better approach is to use mechanisms to constrain interleaving to ensure correct behaviour.

#### Serializing access to shared state

- Serialization groups procedures into sets such and prevents multiple procedures in the same set from executing concurrently.
- We can use this to control access to shared variables.
- Before, assignments based on a state variables current value were problematic. We could solve this with the set {`get-value`, `set-value!`, and `swap-value!`} where the latter is defined like so:

```
(define (swap-value! f)
  (set-value! (f (get-value)))
```

#### Serializers in Scheme

- Suppose we have a procedure `parallel-execute` that takes a variable number of arguments that are procedures of no arguments, and executes them all concurrently.
- We construct _serializers_ with `(make-serializer)`.
- A serializer takes a procedure as its argument and returns a serialized procedure that behaves like the original.
- Calls to the same serializer return procedures in the same set.

#### Complexity of using multiple shared resources

- Serializers are powerful, and easy to use for one resource.
- Things get much more difficult with multiple shared resources.
- Suppose we want to swap the balances in two bank accounts:

```
(define (exchange acc1 acc2)
  (let ((diff (- (acc1 'balance) (acc2 'balance))))
    ((acc1 'withdraw) diff)
    ((acc2 'deposit) diff)))
```

- Serializing deposits and withdrawals themselves is not enough to ensure correctness.
- The exchange comprises four individually serialized steps, and these may interleave with a concurrent process.
- One solution is to expose the serializer from `make-account`, and use that to serialize the entire exchanging procedure.
- We would have to manually serialize deposits, but this would give us the flexibility to serialize the exchanging procedure.

#### Implementing serializers

- Serializers are implemented in terms of the primitive _mutex_.
- A mutex can be _acquired_ and it can be _released_.
- Once acquired, no other acquire operations can proceed until the mutex is released.
- Each serializer has an associated mutex.
- A serialized procedure (created with `(s proc)` where `s` is a serializer) does the following when it is run:
	- acquire the mutex,
	- run the procedure `proc`,
	- release the mutex.
- The mutex is a mutable object, represented by a _cell_ (a one-element list). It holds a boolean value, indicating whether or not it is currently locked.
- To acquire the mutex, we test the cell. We wait until it is false, then we set it to true and proceed.
- To release the mutex, we set its contents to false.

#### Deadlock

- Even with a proper implementation of mutexes and seralizers, we still have a problem with the account exchanging procedure.
- We serialize the whole procedure with both accounts so that an account may only participate in one exchange at a time.
- There are two mutexes, so it is possible for something to happen in between acquiring the first and the second.
- If we exchange `a1` with `a2` and concurrently do the reverse exchange, it is possible for the first process to lock `a1` and the second process to lock `a2`.
- Now both need to lock the other, but they can't. This situation is called _deadlock_.
- In this case, we can fix the problem by locking accounts in a particular order based on a unique identifier.
- In some cases, it is not possible to avoid deadlock, and we simply have to "back out" and try again.

#### Concurrency, time, and communication

- Concurrency can be tricky because it's not always clear what is meant by "shared state."
- It also becomes more complicated in large, distributed systems.
- The notion of time in concurrency control must be intimately linked to _communication_.
- There are some parallels with the theory of relativity.

## Streams

- We've used assignment as a powerful tool and dealt with some of the complex problems it raises.
- Now we will consider another approach to modelling state, using data structures called _streams_.
- We want to avoid identifying time in the computer with time in the modelled world.
- If $x$ is a function of time $x(t)$, we can think of the identity $x$ as a history of values (and these don't change).
- With time measured in discrete steps, we can model a time function as a sequence of values.
- Stream processing allows us to model state without assignments.

### Streams are delayed lists

- If we represent streams as lists, we get elegance at the price of severe inefficiency (time _and_ space).
- Consider adding all the primes in an interval. Using `filter` and `reduce`, we waste a lot of space storing lists.
- Streams are lazy lists. They are the clever idea of using sequence manipulations without incurring the cost.
- We only construct an item of the stream when it is needed.
- We have `cons-stream`, `stream-car`, `stream-cdr`, `the-empty-stream`, and `stream-null?`.
- The `cons-stream` procedure must not evaluate its second argument until it is accessed by `stream-cdr`.
- To implement streams, we will use _promises_. `(delay <exp>)` does not evaluate the argument but returns a promise. `(force <promise>)` evaluates a promise and returns the value.
- `(cons-stream a b)` is a special form equivalent to `(cons a (delay b))`.

#### The stream implementation in action

> In general, we can think of delayed evaluation as "demand-drive" programming, we herby each stage in the stream process is activated only enough to satisfy the next stage. (438)

#### Implementing `delay` and `force`

- Promises are quite straightforward to implement.
- `(delay <exp>)` is syntactic sugar for `(lambda () <exp>)`.
- `force` simply calls the procedure. We can optimize it by saving the result and not calling the procedure a second time.
- The promise stored in the `cdr` of the stream is also known as a _thunk_.

### Infinite streams

- With lazy sequences, we can manipulate infinitely long streams!
- We can define Fibonacci sequence explicitly with a generator:

```
(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
```

- As long as we don't try to display the whole sequence, we will never get stuck in an infinite loop.
- We can also create an infinite stream of primes.

#### Defining streams implicitly

- Instead of using a generator procedure, we can define infinite streams implicitly, taking advantage of the laziness.

```
(define fibs
  (cons-stream
    0
    (cons-stream 1 (stream-map + fibs (stream-cdr fibs)))))
(define pot
  (cons-stream 1 (stream-map (lambda (x) (* x 2)) pot)))
```

- This implicit technique is known as _corecursion_. Recursion works backward towards a base case, but corecursion works from the base and creates more data in terms of itself.

### Exploiting the stream paradigm

- Streams can provide many of the benefits of local state and assignment while avoiding some of the theoretical tangles.
- Using streams allows us to have different module boundaries.
- We can focus on the whole stream/series/signal rather than on individual values.

#### Formulating iterations as stream processes

- Before, we made iterative processes by updating state variables in recursive calls.
- To compute the square root, we improved a guess until the values didn't change very much.
- We can make a stream that converges on the square root of `x`, and a stream to approximate π.
- One neat thing we can do with these streams is use sequence accelerators, such as Euler's transform.

#### Infinite streams of pairs

- Previously, we handled traditional nested loops as processes defined on sequences of pairs.
- We can find all pairs $(i,j)$ with $i ≤ j$ such that $i + j$ is prime like this:

```
(stream-filter
  (lambda (pair) (prime? (+ (car pair) (cadr pair))))
  int-pairs)
```

- We need some way of producing a stream of all integer pairs.
- More generally, we can combined two streams to get a two-dimensional grid of pairs, and we want to reduce this to a one-dimensional stream.
- One way to do this is to use `interleave` in the recursive definition, in order to handle infinite streams.

#### Streams as signals

- We can use streams to model signal-processing systems.
- For example, taking the integral of a signal:

```
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int) 
```

### Streams and Delayed Evaluation

- The use of `delay` in `cons-stream` is crucial to defining streams with feedback loops.
- However, there are cases where we need further, explicit uses of `delay`.
- For example, solving the differential equation $dy/dx = f(y)$ where $f$ is a given function:

```
(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)
```

- This does not work because the definition of `y` and `dy` depend on each other.
- To solve this, we need to change our implementation of `integral`:

```
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
      initial-value
      (let ((integrand (force delayed-integrand)))
	(add-streams (scale-stream integrand dt) int))))
  int)
```
