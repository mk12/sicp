# Frontmatter

## Dedication

> What's in your hands, I think and hope, is intelligence: the ability to see the machine as more than when you were first led up to it, that you can make it more. (xii)

## Foreword

- Three foci: the human mind, collections of computer programs, and the computer.
- Idioms form "an arsenal of standard program structures of whose correctness we have become sure" (xiv).
- Algorithms are programs that perform a precise mathematical function (optimized for execution time and data storage).

> Lisp is for building organisms -- imposing, breathtaking, dynamic structures built by squads fitting fluctuating myriads of simpler organisms into place. (xvii)

> It is better to have 100 functions operate on one data structure than to have 10 functions operate on 10 data structures. (xvii)

## Preface

- Computer language: a novel formal medium for expressing ideas about methodology.

> Thus, programs must be written for people to read, and only incidentally for machines to execute. (xxii)

> Underlying our approach to this subject is our conviction that "computer science" is not a science and that its significance has little to do with computers. (xxiii)

> Mathematics provides a framework for dealing precisely with notions of "what is." Computation provides a framework for dealing precisely with notions of "how to." (xxiii)

# Building Abstractions with Procedures

- A computational process evolves to manipulate data.
- The evolution is controlled by a program, a pattern of rules.
- Well-designed computational systems are modular.
- This book uses the Scheme dialect of Lisp.
- Lisp represents procedures as data.

## The Elements of Programming

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

### Naming and the Environment

- Scheme names things with the `define`. This is the simplest means of abstraction.
- The name-value pairs are stored in an _environment_.

### Evaluating Combinations

- To evaluate a combination, do the following:
    1. Evaluate the subexpressions of the combination.
    2. Apply the procedure (value of left more subexpression, the operator) to the arguments (values of other subexpressions, the operands).
- Before evaluating a combination, we must first evaluate each element inside it.
- Evaluation is recursive in nature -- one of its steps is invoking itself.
- The evaluation of a combination can be represents with a tree.
- Recursion is a powerful technique for dealing with hierarchical, tree-like objects.
- To end the recursion, we stipulate the following:
    1. Numbers evaluate to themselves.
    2. Built-in operators evaluate to machine instruction sequences.
    3. Names evaluate to the values associated with them in the environment.
- Rule 2 is a special case of rule 3 if we consider the arithmetic operators to be names in the environment.
- Evaluating `(define x 3)` does not apply `define` to two arguments; this is not a combination.
- Exceptions such as these are _special forms_. Each one has its own evaluation rule.

> Syntactic sugar causes cancer of the semicolon. (Alan Perlis)

### Compound Procedures

- Procedure definitions are very powerful for abstraction.
- A squaring procedure: `(define (square x) (* x x))`.
- This is a compound procedure given the name _square_.
- The general form of a procedure definition is `(define (<name> <formal parameters>) <body>)`.
- If the body contains more than one expression, each is evaluated in sequence and the value of the last one is returned.

### The Substitution Model for Procedure Application

This is the substation model:

> To apply a compound procedure to arguments, evaluate the body of the procedure with each formal parameter replaced by the corresponding argument.

An example of procedure application:

```scheme
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

```scheme
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

### Conditional Expressions and Predicates

- To make more useful procedures, we need to be able to make tests and perform different operations accordingly.
- We do _case analysis_ in Scheme using `cond`.
- Those conditional expressions work by testing each predicate. The consequent expression of the first clause with a true predicate is returned, and the other clauses are ignored.
- A predicate is an expression that evaluates to true or false, or a procedure that returns true or false.
- The symbol `else` can be used as the last clause -- it will always evaluate to true.
- The `if` conditional can be used when there are two cases.
- Logical values can be combined with `and`, `or`, and `not`. The first two are special forms, not procedures.

### Example: Square Roots by Newton's Method

> But there is an important difference between mathematical functions and computer procedures. Procedures must be effective. (28)

- In mathematics, you can say "the square root of $x$ is the nonnegative $y$ such that $y^2 = x$." This is not a procedure.
- Mathematical functions describes things (declarative knowledge); procedures describe how to do things (imperative knowledge).
- Declarative is _what is_, imperative is _how to_.

### Procedures as Black-Box Abstractions

- Each procedure in a program should accomplish and identifiable task that can be used as a module in defining other procedures.
- When we use a procedure as a "black box," we are concerned with _what_ it is doing but not _how_ it is doing it.
- This is called procedural abstraction. Its purpose is to suppress detail.

> A user should not need to know how the procedure is implemented in order to use it. (35)

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

## Procedures and the Processes They Generate

> To become experts, we must learn to visualize the processes generated by various types of procedures. Only after we have developed such a skill can we learn to reliably construct programs that exhibit the desired behavior. (40)

- A procedure is a pattern for the _local evolution_ of a computation process: how one stage is built on the previous.
- The global behavior of a computational process is much harder to reason about.
- Processes governed by different types of procedures generate different "shapes" of evolution.
- There are two important resources that computational processes consume: time and space.

### Linear Recursion and Iteration

- The factorial of $N$ is defined as the product of the integers on the interval $[1,N]$.
- The naive _recursive_ implementation creates a curved shape:

```scheme
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

```scheme
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

### Tree Recursion

- With tree recursion, the procedure invokes itself more than once, causing the process to evolve in the shape of a tree.
- The naive Fibonacci procedure calls itself twice each time it is invoked, so each branch splits into two at each level.

> In general, the number of steps required by a tree-recursive process will be proportional to the number of nodes in the tree, while the space required will be proportional to the maximum depth of the tree. (49)

- The iterative implementation of the Fibonacci procedure is vastly more efficient in space and in time.

#### Example: Counting change

Let $f(A,N)$ represent the number of ways of changing the amount A using $N$ kinds of coins. If the first kind of coin has denomination $N$, then $f(A,N) = f(A,N−1) + f(A−D,N)$. In words, there are two situations: where you do not use any of the first kind of coin, and when you do. The value of $f(A,N−1)$ assumes we don't use the first kind at all; the value of $f(A−D,N)$ assumes we use one or more of the first kind.

That rule and a few degenerate cases is sufficient to describe an algorithm for counting the number of ways of changing amounts of money. We can define it with the following piecewise function:

$$
f(A,N) = \begin{cases}
1, & \text{if $A = 0$,} \\
0, & \text{if $A < 0$ or $N = 0$,} \\
f(A,N-1) + f(A-D,N), & \text{if $A > 0$ and $N > 0$.}
\end{cases}
$$

Like Fibonacci, the easy tree-recursive implementation involves a lot of redundancy. Unlike it, there is no obvious iterative solution (it is possible, just harder). One way to improve the performance of the tree-recursive process is to use _memoization_ (maintaining a lookup table).

### Orders of Growth

- Some processes consume more or less computational resources than others.
- We compare this using _order of growth_, a gross measure of the resources required by a process as the inputs becomes larger.
- Let $n$ be a parameter that measures the size of a problem -- it could be the input itself, the tolerance, the number of rows in the matrix, etc. There are many properties that $n$ can measure.
- Let $R(n)$ be the amount of resources the process requires for a problem of size $n$. This could be time, space (amount of memory), number of registers used, etc.
- We say that $R(n)$ has order of growth $Θ(f(n))$, or $R(n) = Θ(f(n))$,
if there are positive constants $A$ and $B$ independent of $n$ such that $Af(n) ≤ R(n) ≤ Bf(n)$ for any sufficiently large value of $n$.
- The value $R(n)$ is sandwiched between $Af(n)$ and $Bf(n)$.
- The linear recursive process for computing factorials had $Θ(n)$ time and $Θ(n)$ space (both linear), whereas the linear iterative process had $Θ(1)$ space (constant).
- The order of growth is a crude description of the behavior of a process.
- Its importance is allowing us to see the _change_ in the amount of resources required when you, say, increment $n$ or double $n$.

### Exponentiation

One way to calculate b to the nth power is via the following recursive definition:

$$b^0 = 1, \qquad b^n = b * b^{n-1}.$$

In words, multiply the base to itself $n$ times. A faster method is to use successive squaring:

$$
b^n = \begin{cases}
\left(b^{n/2}\right)^2 & \text{if $n$ is even,} \\
b * b^{n-1}, & \text{if $n$ is odd.}
\end{cases}
$$

### Greatest Common Divisors

- The GCD of integers $a$ and $b$ is the largest integer that divides both $a$ and $b$ with no remainder. For example, $\gcd(16,28) = 4$.
- Efficient algorithm uses $\gcd(a,b) = \gcd(b,a\bmod b)$.
- For example, we can reduce `(gcd 206 40)` as follows:

```scheme
(gcd 206 40)
(gcd 40 6)
(gcd 6 4)
(gcd 4 2)
(gcd 2 0)
2
```

- This always works: you always get a pair where the second number is zero, and the other number is the GCD of the original pair.
- This is called _Euclid's Algorithm_.
- Lamé's Theorem: If Euclid's Algorithm requires $k$ steps to compute the GCD of some pair $(a,b)$, then $\min\{a,b\} ≥ \text{Fib}(k)$.

### Example: Testing for Primality

#### Searching for divisors

- One way to test for primality is to find the number's divisors.
- A number is prime if and only if it is its own smallest divisor.

#### The Fermat test

The Fermat test is a $Θ(log(n))$ primality test based on Fermat's Little Theorem:

> If $n$ is a prime number and a is any positive integer less than $n$, then a raised to the $n$th power is congruent to a modulo $n$.

The test works like this:

1. Given a number $n$, pick a random number $a < n$ and calculate $a^n\bmod n$.
2. Fail: If the result is not equal to $a$, then $n$ is not prime.
3. Pass: If the result is equal to $a$, then $n$ is likely prime.
4. Repeat. The more times the number passes the test, the more confident we are that $n$ is prime. If there is a single failure, $n$ is certainly not prime.

#### Probabilistic methods

- Most familiar algorithms compute an answer that is guaranteed to be correct. Not so with the Fermat test.
- We can make the probability error in our primality test as small as we like simply by running more tests -- except for Carmichael numbers.
- If $n$ passes the test for one random value of a, the chances are more than 50% that $n$ is prime.
- Probabilistic algorithms: algorithms for which one can prove that the chance of error becomes arbitrarily small.

> Numbers that fool the Fermat test are called Carmichael numbers, and little is known about them other than that they are extremely rare. There are 255 Carmichael numbers below 100,000,000. The smallest few are 561, 1105, 1729, 2465, 2821, and 6601. In testing primality of very large numbers chosen at random, the chance of stumbling upon a value that fools the Fermat test is less than the chance that cosmic radiation will cause the computer to make an error in carrying out a "correct" algorithm. Considering an algorithm to be inadequate for the first reason but not for the second _illustrates the difference between mathematics and engineering_. (69)

## Formulating Abstractions with Higher-Order Procedures

> We have seen that procedures are, in effect, abstractions that describe compound operations on numbers independent of the particular numbers. (74)

- We could always use `(* x x x)` instead of using a cube procedure, but this would be a disadvantage.

> One of the things we should demand from a powerful programming language is the ability to build abstractions by assigning names to common patterns and then to work in terms of the abstractions directly. (75)

- Even with procedures, we are still limiting ourselves if we only ever let parameters be numbers.
- To abstract more general programming patterns, we need to write precedes that take other procedures as arguments and return new procedures.
- These are called _higher-order_ procedures.

### Procedures as Arguments

Procedures that compute a sum are all similar. They are all based on the following template:

```scheme
(define (<name> a b)
  (if (> a b)
      0
      (+ (<term> a)
         (<name> (<next> a) b))))
```

This is a useful abstraction, just as sigma notation in math is useful because the summation of a series is so common.

> The power of sigma notation is that it allows mathematicians to deal with the concept of summation itself rather than only with particular sums. (77)

### Constructing Procedures Using `Lambda`

`lambda` creates anonymous procedures. They are just like the procedures created by `define`, but without a name: `(lambda (<formal-parameters>) <body>)`.

A lambda expression can be used as the operand in a combination. It will be evaluated to a procedure and applied to the arguments (the evaluated operands). The name comes from the λ-calculus, which was introduced by Alonzo Church.

#### Using `let` to create local variables

- We often need local variables other than the ones that have been bound as formal parameters.
- We can do this with a lambda expression that takes the local variables as arguments, but this is so common that there is a special `let` form that does it.

The general form of a let-expression is

```scheme
(let ((<var1> <exp1>)
      (<var2> <exp2>)
      ...
      (<varn> <expn>))
  <body>)
```

This is just syntactic sugar for

```scheme
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

### Procedures as General Methods

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

### Procedures as Returned Values

Passing procedures as arguments gives us expressive power; Returning procedures from functions gives us even more. For example, we can write a procedure that creates a new procedure with average damping:

```scheme
(define (average-damp f)
  (lambda (x) (average x (f x))))
```

If we use `average-damp` on `square`, we actually get a procedure that takes the sum of the numbers from 1 to n:

```scheme
((average-damp square) 10)
55
(+ 1 2 3 4 5 6 7 8 9 10)
55
```

> In general, there are many ways to formulate a process as a procedure. Experienced programmers know how to choose procedural formulations that are particularly perspicuous, and where useful elements of the process are exposed as separate entities that can be reused in other applications.

#### Newton's method

The square-root procedure we did earlier was a special case of Newton's method.  Given a function $f(x)$, the solution to $f(x) = 0$ is given by the fixed point of

$$x ↦ x − \frac{f(x)}{f'(x)}.$$

Newton's method converges very quickly -- much faster than the half-interval method in favorable cases. We need a procedure to transform a function into its derivative (a new procedure). We can use a small dx for this:

$$f'(x) = \frac{f(x+dx) - f(x)}{dx}.$$

This translates to the following procedure:

```scheme
(define (deriv f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))
```

Now we can do things like this:

```scheme
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
