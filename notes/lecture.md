These notes are based on the [MIT OpenCourseWare video lectures][ocw] given in July 1986 by Hal Abelson and Gerald Jay Sussman. They follow the first edition of <cite>Structure and Interpretation of Computer Programs</cite>, not the second edition like the [textbook notes][text].

[ocw]: https://ocw.mit.edu/courses/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video_galleries/video-lectures/
[text]: ../text/index.html

# 1A: Overview and Introduction to Lisp

## Part 1

### Computer science

- It's not a science, and it's not _really_ about computers.
- It's not about computers in the same way that astronomy isn't about telescopes.
- The ancient Egyptians began geometry using surveying instruments. We now know that the essence of geometry is much bigger than the act of using these primitive tools.
- We often conflate the essence of a field with its tools.
- In a thousand years, they will look back on us in a similar way to how we look at the ancient Egyptians: "They were playing around with these digital computers, but that was only the beginning of the much broader ideas about computation".

### Declarative vs. imperative

Mathematical declarative statement (what-is knowledge): The square root of $x$ is the $y$ such that $y ≥ 0$ and $y^2 = x$.

Imperative instructions (how-to knowledge): approximate the square root of $x$ with the following steps:

1. Make a guess, $G$.
2. Improve the guess by averaging $G$ and $x/G$.
3. Keep improving until the guess is good enough.

### Processes and Lisp

- The above is an algorithm. More generally, it is a _process_.
- What is a process? It's like a magical spirit that lives in the computer.
- The process is directed by a pattern of rules (magical spell) called a procedure.
- We conjure our spirits in a language called Lisp.
- Lisp is easy to learn just as chess is easy to learn.
- Rules stated in minutes, but there are many implications.
- Much more difficult to become a master programmer: understanding all the implications, knowing how to approach problems.

### Complexity and computer science

::: highlight
> The real problems come when we try to build very, very large systems ... nobody can really hold them in their heads all at once ... the only reason that that's possible is because there are techniques for controlling the complexity of these large systems. And these techniques that are controlling complexity are what this course is really about. And in some sense, that's really what computer science is about. [@1a.p3]
:::

- Computer scientists are in the business of controlling complexity.
- This is different from the complexity that others, for example aeronautical engineers, deal with, because the complexity in CS in a sense is not real.
- Computer science deals with _idealized_ components.
- We know as much as we want about the components; no worrying about tolerance.
- Not much difference between what I can _build_ and what I can _image_.
- Other disciplines have physical constraints; in CS, the only constraint is your mind.

::: highlight
> So in that sense, computer science is like an abstract form of engineering. It's the kind of engineering where you ignore the constraints that are imposed by reality. [@1a.p3]
:::

### Techniques for managing complexity

**Black box abstraction**: Take something and build a box around it. The important thing is that you don't care what is going on inside the box -- it's not important. Black-box abstraction _suppresses detail_. This allows you to go on and build bigger boxes.

- Primitive objects: primitive procedures, primitive data.
- Means of combination: procedure composition, construction of compound data.
- Means of abstraction: procedure definition, simple data abstraction.
- Capturing common patterns: higher-order procedures, data as procedures.

**Conventional interfaces**: Agreed upon ways of connecting things together. Like standard impedances in electrical engineering.

- generic operations
- large-scale structure and modularity
- object-oriented programming
- operations on aggregates

**Metalinguistic abstraction**: Another way of controlling complexity is to choose a new design language (a domain-specific language, or DSL) that will highlight different aspects of the system. It will emphasize some kinds of details and suppress others. This is the technology for building new computer languages.

The process of interpreting Lisp in Lisp is like a giant wheel of two processes, `apply` and `eval`, which reduce expressions to each other. Very magical.

- `apply` and `eval`
- logical programming
- register machines

## Part 2

### Three main features

**Primitive elements**: Here are some primitive elements is Lisp: `3`, `14.4`, `5`, `+`. These are all names that represents things. The first three represent numbers, and the last one represents the concept of addition.

**Means of combination**:

- We can take the sum using a _combination_: `(+ 3 14.4 5)`.
- Combination: applying and operator to operands.
- The operator and operands themselves can be combinations.
- Lisp uses fully parenthesized (unambiguous) prefix notation.
- Parentheses are very different in Lisp and in mathematics.
- Nested combinations can be modeled as trees.
- Parentheses are just a way to write trees as a linear sequence of characters.

**Means of abstraction**: This is accomplished in Lisp with `define`. Defining something gives a name to an expression. We write this the same way as a regular combination, but `define` is not a procedure -- it is a _special form_. We can also define procedures this way:

```
(define (square x) (* x x))
```

We can also make it more clear that we are naming something:

```
(define square (lambda (x) (* x x)))
```

The former notation is just syntactic sugar for the latter: a more convenient surface forms for typing something. The former _desugars_ to the latter.

In Lisp, you do not make arbitrary distinctions between things that are defined in the language and things that happen to be built-in.

### Case analysis

We do case analysis in Lisp using `cond`.

```
(define (abs x)
  (cond ((< x 0) (- x))
        ((= x 0) 0)
        ((> x 0) x)))
```

Each line is a clause consisting of a predicate (true or false) and an action. We can use `if` if there is a single case:

```
(define (abs x)
  (if (< x 0)
      (- x)
      x))
```

You can think of `if` as syntactic sugar for `cond` or vice versa.

### Recursion

- We now know enough to implement any numerical procedure that you could implement in other languages.
- We don't need any looping constructs in Lisp. We can define things in terms of themselves using _recursion_.

### Block structure

- We can create a black box by packaging internals inside of a definition. This is called _block structure_.

# 1B: Procedures and Processes; Substitution Model

## Part 1

### Programs and processes

- The job of a programmer is to design processes that accomplish particular goals, like finding the square root of a number.
- You do this by constructing spells (procedures, expressions) which direct a process to accomplish the desired goal.
- You must understand the relationship between the particular spells you cast and the process you're trying to control.
- How do particular patterns of procedures and expressions cause particular patterns of execution and behavior in the process?

### Kinds of expressions

So far we've seen three main kinds of expressions:

- numbers
- symbols
- combinations

These are also expressions, but they are _special forms_, so we will worry about them later:

- lambdas
- definitions
- conditionals

### Evaluating combinations

These are the substitution rules for evaluating a combination:

1. Evaluate the operator to get the procedure.
2. Evaluate the operands to get the arguments.
3. Apply the procedure to the arguments.
    1. Copy the body of the procedure.
    2. Substitute the arguments supplied for the formal parameters of the procedure.
    3. Evaluate the resulting body.

### Example

The `sos` procedure takes the sum of the squares:

```
(define (sq a) (* a a))
(define (sos x y) (+ (sq x) (sq y)))
```

Let's evaluate the sum of the square of 3 and the square of 4:

```
(sos 3 4)
(+ (sq 3) (sq 4))
(+ (sq 3) (* 4 4))
(+ (sq 3) 16)
(+ (* 3 3) 16)
(+ 9 16)
25
```

This is not a perfect description of what the computer does. But it is a good enough model for now.

::: highlight
> But one of the things we have to learn how to do is ignore details. The key to understanding complicated things is to know what not to look at, and what not compute, and what not to think. [@1b.p3]
:::

### Evaluating conditionals

To evaluate `(if «predicate» «consequent» «alternative»)`, follow these steps:

1. Evaluate the predicate expression.
    1. If it yields true, evaluate the consequent expression.
    2. If it yields false, evaluate the alternative expression.

### Example

The addition operator in Peano arithmetic uses a conditional:

```
(define (+ x y)
  (if (= x 0)
      y
      (+ (-1+ x) (1+ y))))
```

Now we can evaluate `(+ 3 4)` like so:

```
(+ 3 4)
(if (= 3 0) 4 (+ (-1+ 3) (1+ 4)))
(+ (-1+ 3) (1+ 4))
(+ (-1+ 3) 5)
(+ 2 5)
(if (= 2 0) 5 (+ (-1+ 2) (1+ 5)))
(+ (-1+ 2) (1+ 5))
(+ (-1+ 2) 6)
(+ 1 6)
(if (= 1 0) 6 (+ (-1+ 1) (1+ 6)))
(+ (-1+ 1) (1+ 6))
(+ (-1+ 1) 7)
(+ 0 7)
(if (= 0 0) 7 (+ (-1+ 0) (1+ 7)))
7
```

## Part 2

### Pre-visualization

- Programs are made of procedures and expressions, and they evolves processes.
- But how do particular programs evolve particle processes?
- We want to go from particularly shaped programs to particularly shaped processes.
- We want to pre-visualize the process like a photographer pre-visualizes the photo before taking the shot.

### Peano arithmetic

There are two ways to add whole numbers in Peano arithmetic.

```
(define (+ x y)
  (if (= x 0)
      y
      (+ (-1+ x) (1+ y))))
(define (+ x y)
  (if (= x 0)
      y
      (1+ (+ (-1+ x) y))))
```

Both are written using recursion, but they are different: the first generates a _linear iterative_ process with $Θ(n)$ time and $Θ(n)$ space; the second generates a _linear recursive_ process with $Θ(n)$ time and $Θ(1)$ space.

The iteration has all of its state in explicit variables. The recursion does not.

## Part 3

### Perturbation analysis

Make small changes to the program and see how it affects the process.

### Fibonacci sequence

We can represent the Fibonacci numbers 0, 1, 1, 2, 3, 5, 8, 13, 21, ... in Lisp:

```
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
```

This is a _tree-recursive_ process. We can represent the evaluation with a tree. This is a terribly inefficient process because there is so much redundant computation. The time complexity of this is actually the Fibonacci numbers. The space complexity is linear.

>  And the reason why people think of programming as being hard, of course, is because you're writing down a general rule, which is going to be used for lots of instances ... You've got to write down something that's a general in terms of variables, and you have to think of all the things that could possibly fit in those variables, and all those have to lead to the process you want to work. [@1b.p13]

### Tower of Hanoi

::: highlight
> The way in which you would construct a recursive process is by wishful thinking. You have to believe. [@1b.p13]
:::

Move an $n$-high tower from spike `from` to spike `to` using spike `spare` as a spare:

```
(define (move n from to spare)
  (cond
    ((= n 0) "DONE")
    (else (move (-1+ n) from spare to)
          (print-move from to)
          (move (-1+ m) spare to from))))
```

# 2A: Higher-Order Procedures

## Part 1

### Don't repeat yourself

- So far Lisp might just seem like a different language with funny syntax. It's time to shatter that illusion.
- We are familiar with creating recursive procedures that, for example, calculate the sum of integers from `a` to `b`.
- What about the sum of the squares of the integers from `a` to `b`? That's almost the same program! We don't like repetition.

> Now, wherever you see yourself writing the same thing down more than once, there's something wrong, and you shouldn't be doing it. And the reason is not because it's a waste of time to write something down more than once. It's because there's some idea here ... whenever trying to make complicated systems and understand them, it's crucial to divide the things up into as many pieces as I can, each of which I understand separately. [@2a.p2]

- No repetition means you only write it once, but also only understand and debug it once!
- Any time you see things that are almost identical, think of an abstraction to cover them.
- An _idiom_ is a common pattern in a language that is useful to know. In Lisp, we can give idioms names and abstract them.
- There is nothing very special about numbers. Numbers are just one kind of data. Procedures are also data.

### Summation

We can represent sigma notation with a procedure that takes other procedures as arguments:

```
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
```

Now we can write particular cases easily, without repeating ourselves:

```
(define (sum-int a b)
  (define (identity x) x)
  (sum identity a 1+ b))
(define (sum-sq a b)
  (sum square a 1+ b))
(define (pi-sum a b)
  (sum (lambda (i) (/ i (* i (+ i 2))))
       a
       (lambda (i) (+ i 4))
       b))
```

We are separating the things we are adding up from the method of doing the addition. Now, we can change the `sum` procedure so that it generates an iterative process, and all the specific procedures using `sum` will benefit.

## Part 2

### Higher-order procedures

- We use abstraction to make programs easier to read and write.
- Higher-order procedures increase our abstraction capabilities: they can take other procedures as arguments and return new procedures.
- Our square root algorithm was a specific instance of the more general fixed-point search algorithm. Expressing this directly requires higher-order procedures.
- _Damping_ is a general signal processing strategy. Using a higher-order procedure, we can write programs that use damping as a building block.

## Part 3

### Newton's method

- _Newton's method_ is a general technique for finding zeros of a function.
- We can implement Newton's method in terms of the fixed-point procedure.

> Wishful thinking, essential to good engineering, and certainly essential to good computer science. [@2a.p11]

- _Top-down design_ allows us to use names of procedures that we haven't defined yet while writing a program.

### The rights and privileges of first-class citizens

- To be named by variables.
- To be passed as arguments to procedures.
- To be returned as values of procedures.
- To be incorporating into data structures.

# 2B: Compound Data

## Part 1

### Recap

In the beginning, we learned about

- primitive forms
- means of combination
- means of abstraction

Then, we learned how to use higher-order procedures to represent general methods of computation. This gave us extraordinary expressive power.

### Layered system

- When we wrote the square root procedure, we used layers of abstraction. Someone else could have written `good-enough?` for us without understanding the rest.

> So the crucial idea is that when we're building things, we divorce the task of building things from the task of implementing the parts. And in a large system, of course, we have abstraction barriers like this at lots, and lots, and lots of levels. [@2b.p1]

- Now we will look at the same issues for data. There are means of combination for data as well, allowing us to combine primitive data into compound data.
- We will also see a methodology for abstraction with data.
- The key idea is to build the system in layers, with abstraction barriers.

### Rational number arithmetic

- We already know how to express the arithmetic operators for fractions in math.
- Adding, subtracting, multiplying, or dividing two fractions produces another fraction.
- The computations are easy -- but how to we represent a fraction?
- We need to apply the strategy of wishful thinking: let's imagine that we have procedures `make-rat`, `numer`, and `denom`.
- We can implement a procedure for adding rationals like so:

```
(define (+rat x y)
  (make-rat
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))
```

- The procedure `make-rat` is called a _constructor_.
- The procedures `numer` and `denom` are called _selectors_.

### Why do we need compound data?

- Why bother with these data objects instead of just passing in four numbers?
    - We would have to worry about all these temporary numbers.
    - It doesn't scale. It's confusing. We need abstraction.
- We need to create compound data objects for the same reason that we separate our programs into procedures built on abstractions.

## Part 2

### Pairs

- How do we make one of these "clouds"? We need a kind of glue to connect things.
- Lisp provides this: it is called _list structure_. It starts with the primitive procedure `cons`.
- `cons` is obviously the **cons**tructor. `car` and `cdr` are the selectors for pairs.
- For any `x` and `y`, `(car (cons x y))` is `x`, and `(cdr (cons x y))` is `y`.
- We can represent pairs with two boxes side by side with an arrow coming from each. This is called _box-and-pointer notation_.

### Lowest terms

- When we use the system to add a half and a quarter, it gives us six eighths instead of three quarters. This isn't what we want.
- This isn't the addition procedure's problem; the `make-rat` procedure should be responsible for reducing to lowest terms.
- We can use the greatest common divisor to fix this:

```
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
```

- Now our fractions will always be reduced to lowest terms.

### Abstraction layer

- The important thing with our rational arithmetic system is the abstraction layer.
- We have the rational arithmetic operators on one side and the pair constructor and selectors on the other. The abstraction barrier between the two is formed by the procedures `make-rat`, `numer`, and `denom`.
- We always want to separate _use_ from _representation_.
- This methodology is called _data abstraction_.

### Why use data abstraction?

- We didn't have to do it this way.
- We could have used `cons`, `car`, and `cdr` directly in the rational arithmetic procedures.
- Is this talk of data abstraction just self-righteous BS?
- Maybe it would be marginally more efficient to skip the data abstraction!
- It goes back to naming. If you have the name of the spirit, you have control over it.
- One advantage is the flexibility to use alternative representations.
- If we use `cons` directly, we would have to reduce to lowest terms every time we make a rational number. Using data abstraction, we can write the code once in the constructor.
- Data abstraction lets us postpone decisions.

### Designing systems

::: highlight
> See, in general, as systems designers, you're forced with the necessity to make decisions about how you're going to do things, and in general, the way you'd like to retain flexibility is to never make up your mind about anything until you're forced to do it. The problem is, there's a very, very narrow line between deferring decisions and outright procrastination. So you'd like to make progress, but also at the same time, never be bound by the consequences of your decisions. [@2b.p10]
:::

::: highlight
> I said that computer science is a lot like magic, and it's sort of good that it's like magic. There's a bad part of computer science that's a lot like religion. And in general, I think people who really believe that you design everything before you implement it basically are people who haven't designed very many things.
>
> The real power is that you can pretend that you've made the decision and then later on figure out which one is right, which decision you ought to have made. And when you can do that, you have the best of both worlds. [@2b.p11]
:::

## Part 3

### Data abstraction

- Data abstraction is a way of controlling complexity in large systems.
- Like all ways of controlling complexity, the real power comes from their use as building blocks for more complicated things.

### Points and line segments

- We could also use pairs to represent points on a plane. The constructor and accessors could use `cons`, `car`, and `cdr`, just like in the rational number system.
- We could use points as a building block to make line segments.
- Now we have a multi-layered system: segments, points, and pairs are all separated.
- Without data abstraction, the procedure for calculating the length of a line segment is very hard to read; worse, it locks you into decisions about representation.
- `cons` can combine anything, not just numbers. With line segments, we combine pairs.
- _Closure_ means we can make pairs of pairs, not just pairs of numbers. We say that the means of combination _closes over_ the things that it makes.

## Part 4

### Abstract data and contracts

- We've done a few  examples. Now we're going to discuss what it _means_ -- much harder.
- Earlier, we assumed the constructors and selectors for rational numbers existed (without knowing about pairs).
- We had defined a rational number representation in terms of _abstract data_.
- We had a contract that the procedures have to fulfill: given a rational number `x` created with `(make-rat n d)`, we must have `(= (/ (numer x) (denom x)) (/ n d))`.
- This tells us whether three procedures are suitable as a basis for rational numbers.

### Implementation of pairs

- Rational numbers _really_ are just this contract, this axiom.
- They might be realized as pairs in a particular implementation, but that has nothing to do with what pairs really are.
- Pairs are similar: they happen to satisfy the contract that `(car (cons x y))` is `x` and `(cdr (cons x y))` is `y`.
- We can implement pairs with procedures. We don't even need special primitives -- all we need are lambdas:

```
(define (cons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))
(define (car x) (x 1))
(define (cdr x) (x 2))
```

- All we need to do is show that this satisfies the axiom. We can do that with the substitution model. It works.
- You couldn't tell if `cons`, `car`, and `cdr` were implemented in this way or not.

### Conclusion

- We don't need data at all for data abstraction: we can do everything with procedures.
- This blurs the line between code and data.
- Procedures are not just the act of doing something.
- Procedures are conceptual entities or objects.

# 3A: Henderson Escher Example

## Part 1

### Recap

- Now we know about procedural abstraction and data abstraction.
- We can isolate the way data objects are _used_ from the way that they are _represented_.
- We learned how to make pairs with `cons`, `car`, and `cdr`.
- We can glue together arbitrary things with `cons`, not just numbers.
- We learned about closure: the things that we combine can themselves be combined. This allows us to build complexity.

### Lists

- There are a lot of different ways of building list structure.
- Lisp has a convention for representing a sequence as chained pairs, called a _list_.
- The `car` of the pair is the first item. The `cdr` of the pair is the rest of the sequence.
- The `cdr` of the last pair has a special marker. This is the empty list, also called nil or null. It is printed as `()`. The predicate `null?` checks if a list is empty (is nil).
- Lisp has a procedure called `list`, which is just an abbreviation for the nested pairs.

### Mapping lists

- It is common to write a procedure that does the same thing to each item in a list and returns a new list of transformed items.
- The recursive strategy is to apply the operation to the `car` of the list, and then `cons` that onto the rest of the list which has already been mapped (wishful thinking).
- Rather than doing this manually each time, we use the higher-order procedure `map`.
- Thinking in terms of operations on aggregates is a powerful idea. It liberates us from worrying about recursion or other details.
- `for-each` is like map, but it doesn't build up a new list. It is just for side-effects.

## Part 2

### Henderson Escher example

- We will build a language for describing self-similar, fractal-like figures.
- This example will blur the line between procedures and data.
- There is only one primitive: a painter. A painter draws its image within a frame.
- We have means of combination: `rotate`, `beside`, and `below`.
- Thanks to the closure property, we can build up complexity in this language quickly.
- A frame is a parallelogram defined by an origin vector and two side vectors.
- All frames are based on transformations from the unit square.
- We can build primitive painters from lists of line segments.
- A painter is a procedure that takes a frame and draws its image within the frame.

## Part 3

### Importance of closure

- Representing painters as procedures is nice because the means of combination just falls out, and you automatically get the closure property.
- The procedures `beside` and `below` are trivial to write. `rotate` is similarly simple.
- The real punchline comes when you look at the means of abstraction. Since painters are just procedures, everything that Lisp supplies for procedures is automatically available to us in this painting language.
- We can write recursive painters without ever having purposely built recursion into the painting language. We can even write higher-order painters.

### The power of Lisp

- The difference between merely implementing something in a language and _embedding_ something in the language: you don't lose the original power of the language.

::: highlight
> Lisp is a lousy language for doing any particular problem. What it's good for is figuring out the right language that you want and embedding that in Lisp. That's the real power of this approach to design. [@3a.p14]
:::

- There is no difference between procedures and data.

### Software engineering

- The methodology (or mythology) of software engineering is this: figure out your task, break it into three sub-tasks, and repeat for each sub-task. Work your way up to the top and you'll end up with this beautiful edifice.
- Each of these nodes in the tree is supposed to fit perfectly into the whole thing.
- The Henderson example didn't work like that. Instead, we had a sequence of layers of language. Each layer depended on the layers beneath it, but it couldn't see their details because there was an abstraction barrier in the way.

> So what you have is, at each level, the objects that are being talked about are the things that were erected at the previous level. [@3a.p16]

- With the top-down tree, each part does a specific task.
- In the Henderson example, we had a full range of linguistic power at each level. Each level did a whole range of things, not a single task.
- This makes the system more robust: easier to adapt to changes.
- A small change in the top-down tree might cause the whole thing to fall down.
- We are talking about levels of language rather than a strict hierarchy. Each level has its own vocabulary.

::: highlight
> The design process is not so much implementing programs as implementing languages. And that's really the powerful idea of Lisp. [@3a.p17]
:::

# 3B: Symbolic Differentiation; Quotation

## Part 1

### Recap

::: highlight
> In order to make a system that's robust, it has to be insensitive to small changes, that is, a small change in the problem should lead to only a small change in the solution. There ought to be a continuity. The space of solutions ought to be continuous in this space of problems. [@3b.p1]
:::

- Don't solve a particular problem at each level: solve a class of problems in the neighborhood of the particular problem by building a language suited to them.
- We've seen the power of embedding languages.
- We're going to confuse the distinction between procedures and data even more badly!

### Derivatives and integrals

- Instead of computing a numerical approximation of the derivative, we want to apply the rules of calculus to algebraic expressions.
- Producing derivatives are easy. Producing integrals, the inverse operation, is hard.
- Why is it easy to go one way but not the other? Because the rules for differentiation are reduction rules.

### Differentiation procedure

- We can write the rules of differentiation as a case analysis that uses recursion.
- But first, we need to be able to represent the expressions.
- Taking an expression as an argument is very different from taking a function.
- You can't open up a function. You can only get answers from it.
- An expression is data representing the algebraic expression that defines a function.
- We can write the procedure `deriv` using lots of wishful thinking.

### Expression representation

- To represent sums, products, quotients, etc., why not use the same language we're writing the program in? We can just use list structure.
- There are more built-in predicates we haven't seen yet: `atom?` and `eq?`.

### Quotation

- We are also introducing _quotation_. The words of English are ambiguous, so we need punctuation: "say your name" is different from "say 'your name'".
- The notation in Lisp is to precede the expression with a single quotation mark.
- Quotation makes a language more complex because we can no longer substitute equals for equals (see Bertrand Russell's "On Denoting").
- The names `car` and `cdr` have survived (originally referring to the address register and the decrement register) because we can use shorthands like `cadddr` and `cadar`.

## Part 2

### Simplifying expressions

- The expressions we get from the `deriv` procedure are ugly.
- Nothing is simplified, and it's hard to read.
- The solution is to change the representation to add a simplification step.

> So the way we might solve this problem is ... change the representation ... it's one of the pieces of artillery we have in our war against complexity. [@3b.p10]

- We have an abstraction barrier between the rules of differentiation and the representation of algebraic expressions.

### Conclusion

- Quotation stops and says, "I'm talking about this expression itself".
- We can write languages that are not only embedded in Lisp, but that are completely different, using quotation. Quotation gives us tremendous power.

# 4A: Pattern Matching and Rule-Based Substitution

## Part 1

### Recap

- [Last time](@3b) we wrote a program to differentiate symbolic mathematical expressions represented using quotation.
- To implement the differentiation rules, we wrote them in a highly stylized form based on conditional dispatch on the types of expressions.
- Why did we have to translate the rules of differential calculus into the language of the computer? Is there some other, more clear way of writing this program?

### Rules

- We follow rules to differentiate expressions. What is a rule, exactly?
- A rule has a _pattern_ on the left and a _skeleton_ on the right.
- We match the source expression to the pattern, and following the rule, we instantiate the skeleton to get the target expression.
- We want to build a language that allows us to directly express these rules. We will work bottom-up, like before.

### Representation

- We can represent the differentiation rules like this:

```
(define deriv-rules
  '(((dd (?c c) (? v)) 0)
    ((dd (?v v) (? v)) 1)
    ((dd (?v u) (? v)) 0)
    ((dd (+ (? x1) (? x2)) (? v))
     (+ (dd (: x1) (: v))
        (dd (: x2) (: v))))
    ((dd (* (? x1) (? x2)) (? v))
     (+ (* (: x1) (dd (: x2) (: v)))
        (* (dd (: x1) (: v)) (: x2))))
    ((dd (** (? x) (?c n)) (? v))
     (* (* (: n)
           (** (: x) (: (- n 1))))
        (dd (: x) (: v))))))
```

- It could be prettier, but that doesn't matter. What matters is that we are writing rules directly in our language.
- `deriv-rules` is a list of rules. Each rule has the form `(«pattern» «skeleton»)`.
- We have invented two concepts for our language:
  - The forms beginning with question marks are called _pattern variables_.
  - The forms beginning with colons are called _substitution objects_.
- Once we have this language, we can reuse it for other things. For example, we can write rules for algebraic simplification:

```
(define algebra-rules
  '((((? op) (?c e1) (?c e2))
     (: (op e1 e2)))
    (((? op) (? e1) (?c e2))
     ((: op) (: e2) (: e1)))
    ((+ 0 (? e)) (: e))
    ((* 1 (? e)) (: e))
    ((* 0 (? e)) 0)))
```

### Syntax

Patterns:

- `foo` matches exactly the symbol `foo`.
- `(a b)` matches a list whose first element matches the pattern `a` and whose second element matches the pattern `b`. This generalizes to lists of any length.
- `(? x)` matches anything, and binds it to `x`.
- `(?c x)` matches a constant, and binds it to `x`.
- `(?v x)` matches a variable, and binds it to `x`.

Skeletons:

- `foo` instantiates to exactly the symbol `foo`.
- `(a b)` instantiates to a list whose first element is the instantiation of `a` and whose second element is the instantiation of `b`. This generalizes to lists of any length.
- `(: x)` instantiates to the value bound to `x` in the pattern.

### Simplification process

- We can imagine the rules as a deck of cards, each one with a pattern and a skeleton.
- The patterns feed into the matcher, and the skeletons feed into the instantiator.
- The matcher gives the instantiator a mapping from pattern variables to values.
- The output of the instantiator goes back into the matcher.
- All of the rules must be tried on the expression, and on all its subexpressions. We can stop when it no longer changes.
- If you don't write your rules carefully, there is a danger of going into an infinite loop.

## Part 2

### Matcher

- The matcher takes an expression, a pattern, and a dictionary as input. It outputs another, augmented dictionary.

```
(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed)
        ((atom? pat)
         (if (and (atom? exp) (eq? pat exp))
             dict
             'failed))
        ((arbitrary-constant? pat)
         (if (constant? exp)
             (extend-dict pat exp dict)
             'failed))
        ((arbitrary-variable? pat)
         (if (variable? exp)
             (extend-dict pat exp dict)
             'failed))
        ((arbitrary-expression? pat)
         (extend-dict pat exp dict))
        ((atom? exp) 'failed)
        (else
         (match (cdr pat)
                (cdr exp)
                (match (car pat)
                       (car exp)
                       dict)))))
```

- Consider the pattern `(+ (* (? x) (? y)) (? y))`. We could draw this as a tree.
- The expression matching it, `(+ (* 3 x) x)`, has a very similar tree structure.
- We want to traverse both trees simultaneously and compare them.
- The matcher can fail at any point. The first clause checks and propagates the failure.
- If the pattern is an atom, we make sure the expression is the same atom.
- If the pattern is an arbitrary expression variable like `(? x)`, we simply add the expression to the dictionary.
- If the pattern is an arbitrary constant or variable, we make sure the expression is the correct one before adding to the dictionary.

### Instantiator

- The instantiator takes a dictionary and a skeleton as input and outputs an expression.

```
(define (instantiate skeleton dict)
  (define (loop s)
    (cond ((atom? s) s)
          ((skeleton-evaluation? s)
           (evaluate (eval-exp s) dict))
          (else (cons (loop (car s))
                      (loop (cdr s))))))
  (loop skeleton))
```

- We do a recursive tree walk on the skeleton.
- It's simpler than the matcher because the parts of the tree can be instantiated independently (we don't change the dictionary).
- The only reason for `loop` is to avoid passing `dict` through every recursive call.
- The skeleton evaluation forms like `(: x)` using `evaluate`:

```
(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply (eval (lookup (car form) dict)
                   user-initial-environment)
             (mapcar (lambda (v) (lookup v dict))
                     (cdr form)))))
```

- For atoms (variable names) we simply look them up in the dictionary.
- For more complicated expressions, we lookup and evaluate the `car` (operator) and apply it to the result of looking up everything from the `cdr` (operands).
- This is magic. We don't haven't learned about `apply` and `eval` yet. We will later.

## Part 3

### Plan

- We've seen the two halves of the rule system: the matcher and the instantiator.
- Now we'll create the control structure by which rules are applied to expressions.
- We want to apply all of the rules to every node.
- We call the basic idea "garbage in, garbage out".

### Simplifier

- We want to be able to evaluate `(simplifier rules)` and get a procedure that simplifies expressions according to `rules`.
- For example, we could then write `(define dsimp (simplifier deriv-rules))`.

```
(define (simplifier the-rules)
  (define (simplify-exp exp)
    (try-rules (if (compound? exp)
                   (simplify-parts exp)
                   exp)))
  (define (simplify-parts exp)
    (if (null? exp)
        '()
        (cons (simplify-exp (car exp))
              (simplify-parts (cdr exp)))))
  (define (try-rules exp)
    (define (scan rules)
      (if (null? rules)
          exp
          (let ((dict (match (pattern (car rules))
                             exp
                             (empty-dictionary))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-exp
                 (instantiate (skeleton (car rules))
                              dict))))))
    (scan the-rules))
  simplify-exp)
```

- The procedures `simplify-exp` and `simplify-parts` call each other recursively.
- Since the `exp` in `simplify-exp` can be either atomic or compound, this naturally recurses through a tree of expressions.
- We could have just written one procedure, using `map`:

```
(define (simplify-exp exp)
  (try-rules
   (if (compound? exp)
       (map simplify-exp exp)
       exp)))
```

- This is a different idiom; it works the same way, but you think about it a bit differently.
- It's better because otherwise we are basically reinventing the wheel by hiding a definition of `map` in one of our procedures. It is best to extract those common patterns.
- The `try-rules` procedure scans the rules and tries them one by one. It returns the original expression if none matched.
- When there's a match, it instantiates the skeleton and tries to simplify that further.

### Complexity

- The pattern of recursions here is very complicated.
- We shouldn't try to think about it all at once. Instead, we make a modular system where we can focus on one part at a time.
- As long as we know what a procedure application is supposed to do, we can use it without thinking about how it will work.

::: highlight
> The key to this -- very good programming and very good design -- is to know what not to think about. [@4a.p16]
:::

### Dictionaries

- The implementation of the dictionary is pretty simple:

```
(define (empty-dictionary) '())
(define (extend-dictionary pat dat dict)
  (let ((name (variable-name pat)))
    (let ((v ((assq name dict))))
      (cond ((null? v)
             (cons (list name dat) dict))
            ((eq? (cadr v) dat) dict)
            (else 'failed)))))
(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (null? v) var (cadr v))))
```

- When extending the dictionary, we check if the name is already present.
- It it's not present, we add it. If it's present but has the same value, we do nothing. Otherwise it has a conflicting value, and we fail.

# 4B: Generic Operators

## Part 1

### Limits of data abstraction

- So far, we've talked a lot about data abstraction. We've had these horizontal abstraction barriers that separate use from representation.
- This is powerful, but not sufficient for really complex systems.
- The problem is that sometimes we to use need multiple, incompatible representations.
- We want some kind of vertical barrier in addition to the horizontal barriers.
- We want _generic operators_ that work on multiple data representations.
- It should be easy to add new data types to the system, so that the generic operators work on them too with minimal changes.

### Complex number arithmetic

- We can represent a complex number rectangular form or in polar form.
- The rectangular form $x + yi$ and the polar form $re^{iθ}$ are related:
$$\begin{aligned}
x &= r\cos θ, & r &= \sqrt{x^2+y^2}, \\
y &= r\sin θ, & θ &= \arctan(y, x).
\end{aligned}$$
- Adding is easy in rectangular form: just add up the real and imaginary parts.
- Multiplying is easy in polar form: just multiply the magnitudes and add the angles.

### Incompatible representations

- We could expose rectangular and polar selectors but still use one particular representation under the hood. This is nothing new.
- What if we want _both_ representations? Data abstraction allows us to postpone the representation decision, but we don't want to make a decision at all!
- We need a vertical barrier between rectangular and polar forms.
- The selectors `real-part`, `imag-part`, `magnitude`, and `angle` must be generic procedures that work with either representation.
- For this to work, we need _typed data_. We need to tag our data objects with labels telling us the type of their contents.
- We can simply `cons` the symbol `'rectangular` or `'polar` to the complex number data.
- The generic procedures check the type of their argument, strip off the type information, and dispatch the contents to the appropriate specific procedure.

## Part 2

### Problems with the manager

- The strategy we just looked at is called _type dispatch_.
- One annoyance is that we had to rename specific procedures to avoid naming conflicts.
- We'll talk about namespaces to fix that problem later.
- What happens when you add a new type to the system?
    - The other types don't care. They can remain the same.
    - But the manager needs to add a new clause to every generic procedure that should be able to work with the new type.
    - This is annoying because generic procedure case analyses are very repetitive.

### Data-directed programming

- Our system has a table with types (horizontal axis) and operators (vertical axis).
- Instead of writing these generic procedures manually, we should just use a table.
- We introduce two new procedures: `(put «key1» «key2» «value»)` and `(get «key1» «key2»)`.
- Now we just need to insert our specific procedures into the table using `put`, and the rest will be automated.
- It's the procedures that go in the table, not their names, so we could even pass a lambda expression and not give it a name.
- The key procedure in this whole system is `operate`:

```
(define (operate op obj)
  (let ((proc (get (type obj) op)))
    (if (null? proc)
        (error "undefined operator")
        (proc (contents obj)))))
```

- This uses the table to look up the correct procedure, and applies it to the given object.
- Here's what happens when we extract the real part of a complex number in polar form:

```
(real-part z)
(operate 'real-part z)
((get 'polar 'real-part) (contents z))
(real-part-polar '(1 . 2))
(* 1 (cos 2))
-0.4161468365
```

- This style of programming called _data-directed_ programming.
- The data objects themselves carry information about how you should operate on them.

## Part 3

### Generic arithmetic system

- We just looked at data-directed programming for complex numbers.
- The power of the methodology only becomes apparent when you embed this in a more complex system.
- Let's consider a generic arithmetic system with operations `add`, `sub`, `mul`, and `div`.
- This should sit on top of ordinary Lisp numbers, rationals, and complex numbers.
- We already made a rational number package. We just need to change the constructor `make-rat` so that it attaches the tag `'rational` to the data.
- We can't use `operate` anymore because it was designed for a single argument. The `apply-generic` procedure from [](@2.4.3) works for multiple arguments.
- Now, our complex numbers will have two levels of type tags: `'complex` on top and either `'rectangular` or `'polar` underneath.
- At each level, we strip off a type tag and pass the data down to the level beneath. The chain of types leads you down.

### Polynomials

- We can also add polynomials to the generic arithmetic system.
- Our polynomials will have a variable (symbol) and a term list (list of ordered pairs).
- We can add polynomials in the same variable by combining their term lists.
- By using the generic `add` when we add term lists, we can use _any_ kind of number as a coefficient, for free! Including polynomials themselves!
- In other words, all because we wrote `add` instead of `+`, we have this recursive tower of types: the coefficients can be polynomials all the way down, or as far as we'd like.
- If we use the generic arithmetic procedures in the rational number package, we can get rational functions (polynomials over polynomials) for free as well.

### Conclusion

- We built a system that has decentralized control.
- We don't have to worry about how operations are actually performed.
- This lets us build this complex hierarchy where all the operations sort of do the right thing automatically.
- The true complexity comes in with _coercion_: when you add a complex number and a rational, who worries about converting what?

# 5A: Assignment, State, and Side-Effects

## Part 1

### Introducing assignment

- We've learned enough about programming in Lisp to do fairly complex things.
- We have done all of this without an assignment statement.
- Today, we're going to add the assignment statement.
- If we can do so much without it, why should we add it?
- After all, we only add something to the language if we have a good reason for doing so.
- We are adding assignment because it will allow us to break some problems up into certain sets of pieces, and this new means of decomposition would be impossible without assignment.

### Functional programming

- Functional programs encode mathematical truths.
- Processes evolved by these programs can be understood by substitution, which preserves truth (equals for equals).
- Methods may be distinguished by the choice of truths expressed.
- Although functional, Lisp is not quite the same as math. Math is declarative, but Lisp is imperative.

### Assignment and Time

- To do assignment, we use `(set! «variable» «value»)`.
- By convention, we suffix a bang to the names of procedures that do assignment-like things.
- `set!` is like `define`, but the latter is only used once to create the variable in the beginning; the former can only be used on existing variables.
- When we add assignment, we also have to consider _time_.
- The assignment produces a moment in time: a difference between a _before_ and an _after_ in time.
- After the moment, `«variable»` has the value `«value»`, independent of what value it had before.
- So far, calling procedures with the same inputs always produced the same outputs. These procedures are functions.
- This isn't the case when we have assignment. The same expression can lead to different answers because it depends on time.
- The substitution model completely breaks down. It is static.
- We need a new model of computation for this "bad thing". We had better have a good reason for introducing assignment.

### Identity

- Symbols for variables no longer refer directly to their values, but to some _place_.
- A variable now refers to an _identity_. This is an entity associated with a series of causally related values over time.
- The state of an identity is its current associated value.
- When we talk about _time_, we mean the before/after ordering of causal values.

### Factorial example

- Here's how we implemented factorial functionally:

```
(define (fact n)
  (define (iter p i)
    (if (> i n) m (iter (* i p) (+ i 1))))
  (iter 1 1))
```

- We could instead write it with assignments:

```
(define (fact n)
  (let ((p 1) (i 1))
    (define (loop)
      (cond ((> i n) p)
            (else (set! p (* i p))
                  (set! i (+ i 1))
                  (loop))))
    (loop)))
```

- There are ways of making errors in the second program that simply didn't exist when we couldn't do assignment.
- If we change the ordering of the assignments, the procedure calculates the wrong value.

## Part 2

### Environment model

- We need a new model of computation that can allow us to understand programs that use assignment.
- The new model is called the _environment model_.
- Unfortunately, it is significantly more complicated than the substitution model.

### Free and bound variables

- A variable `v` is _bound_ in an expression `E` if the meaning of `E` is unchanged by the uniform replacement of a variable `w` not occurring in `E`, for every occurrence of `v` in `E`.
- We use bound variables all the time.
- The `lambda` is the essential thing that binds variables.
- If a variable isn't bound, we say it is a _free_ variable.
- If `x` is a bound variable in `E` then there is λ-expression where it is bound.
- The list of formal parameters is called the _bound variable list_. The λ-expression _binds_ the variables _declared_ in its bound variable list.
- The parts of the expression where a variable has a value defined by the λ-expression binding it is the _scope_ of the variable.

### Environments, frames, and bindings

- Environments are a way of doing substitutions virtually.
- It's the place where the names of the variables are associated with values.
- An environment is a pointer to a _frame_. A frame is a table of _bindings_, plus a parent/enclosing environment.
- A binding is a variable name associated with a value.
- We look up the value of a variable in an environment by traversing up the frames, finding the first one that has a binding for the variable, and reading its associated value.
- If a variable is bound twice in an environment, the binding in the earlier (lower) frame _shadows_ the other.

### Procedure creation and application

- A procedure is a pair: some code, and (a pointer to) an environment. Free variables are found in this environment.
- Procedure objects are created by evaluating λ-expressions.
- A λ-expression is evaluated relative to a given environment as follows: a new procedure object is formed, combing the text (code) of the λ-expression with (a pointer to) the environment of evaluation.
- A procedure is applied to a set of arguments by
    1. constructing a frame whose parent is the environment part of the procedure being applied,
    2. binding the formal parameters of the procedure to the actual arguments of the call,
    3. evaluating the body of the procedure in the context of the new environment.
- In other words we are taking the environment where the procedure was created, extending it with the frame for the parameters, and evaluating the body.

## Part 3

### Counter example

- We've introduced a very complicated thing, assignment, which destroys most of the interesting mathematical properties of our programs.
- We wouldn't have done this if there wasn't a good reason.
- Consider the following program:

```
(define make-counter
  (lambda (n)
    (lambda ()
      (set! n (inc n))
      n)))
(define c1 (make-counter 0))
(define c2 (make-counter 10))
(c1)
=> 1
(c1)
=> 2
(c2)
=> 11
(c2)
=> 12
```

- These two counters are independent and have their own state.
- The environment of `c1` points to some frame with `n`. That of `c2` points to a different frame with a different `n`. The parent of both frames is the global environment.
- The _global environment_ is the topmost environment. Its frame contains `car`, `+`, `*`, etc., and all top-level definitions.

### Objects

- We like to think about the world as being made up of independent objects, each having their own state.
- The only way to see if two objects are the same (not just associated with an equal value) is to change one and see if the other changes in the same way.
- The idea of objects, changed, and sameness raises deep problems.

::: highlight
> For example, here I am, I am a particular person, a particular object. Now, I can take out my knife, and cut my fingernail. A piece of my fingernail has fallen off onto the table. I believe I am the same person I was a second ago, but I'm not physically the same in the slightest. I have changed. Why am I the same? What is the identity of me? I don't know. Except for the fact that I have some sort of identity. And so, I think by introducing assignment and objects, we have opened ourselves up to all the horrible questions of philosophy that have been plaguing philosophers for some thousands of years about this sort of thing. It's why mathematics is a lot cleaner. [@5a.p14]
:::

### Actions and identity

- An action $A$ had an effect on an object $x$ (or equivalently, that $x$ was changed by $A$) if some property $P$ which was true of $x$ before $A$ became false of $x$ after $A$.
- Two objects $x$ and $y$ are the same if any action which has an effect on $x$ has the same effect on $y$.

### Uses of objection orientation

- One of the nice things about objects is the way it lets us model the world. We like to think of the world as being made up of these independent objects.
- If we want to understand the program well and we want small changes in the world to lead to small changes in the program, we would like isomorphisms between the world and the model.
- For most things, objects and assignment are not the right way to think. We should only use them if we need them.
- Sometimes, though, they are essential.
- It can also improve modularity greatly -- consider the procedure for a random number generator. Without an internal feedback loop via assignment, its state leaks out everywhere and we can't decompose certain problems very well.

# 5B: Computational Objects

## Part 1

### Recap

- We're going to do some complicated programming to illustrate what you can do with mutable state.
- We were motivated by the need to model physical systems.
- Programming this way buys us modularity, if done correctly.

### Electrical systems

- We are going to model electrical systems.
- We can consider the things wires are connected to as objects.
- The connections can be made abstract. We use ideal wires.
- We create wires with `(make-wire)`, and we connect them with gates: `and-gate`, `or-gate`, and `inverter`.
- `(inverter a b)` connects wires `a` and `b` with an inverter.
- Our language will be embedded in Lisp, like the Henderson picture language.
- We can wire up half adders and full adders with these gates.
- We have primitives (wires), means of combination (gates), and means of abstraction (lambda). Lambda is the ultimate glue.

### Implementing the primitives

- All we have to do is implement the primitives. The rest is free.
- An inverter works by adding an action to its input wire. When a new signal arrives, this action delays and then sets the signal of its output wire to the logical-not of its input signal.
- And-gates and or-gates are implemented similarly. The action must be added to both input wires.
- Each wires must remember its signals and its list of actions.
- The actions procedures need to keep references to their wires. These are captured by the lambda inside the gate procedures.
- We implement wires with the message-passing style. It responds to `get-signal`, `set-signal!`, and `add-action!`.
- The wire executes all actions after its signal is set.

### Scheduling delayed tasks

- The agenda is a schedule of actions to run. Whenever `after-delay` is used in an action, a task is added to the agenda.
- These get executed in the proper order, with a time variable maintaining the correct time.
- The `propagate` procedure executers everything the agenda, emptying it as it goes.

## Part 2

### Implementing the agenda

- We've made a simulation where objects and state changed in the computer match the objects and state changes in the world.
- We're going to use agendas (priority queues) to organize time.
- We have a constructor `make-agenda`; selector `current-time` and `first-item`; predicate `empty-agenda?`; and mutators `add-to-agenda!` and `remove-first-item!`.
- We will represent it as a list a segments. Each segment is a time `cons`ed to a queue of tasks (procedures of no arguments).
- To add a task, we either add it to the queue of the appropriate segment, or create a new segment with that time.

#### Queues

- We construct queues with `(make-queue)`.
- There are two mutators, `(insert-queue! q x)` and `(delete-queue! q)`; a selector, `(front-queue q)`; and a predicate, `(empty-queue? q)`.
- A queue is represented as a front pointer of a list `cons`ed to the rear pointer of the same list.
- The mutators use `set-car!` and `set-cdr!`.

## Part 3

### Identity of pairs

- Before, all we needed to know about `cons` was that for all values of `x` and `y`, `(= x (car (cons x y)))` and `(= y (cdr (cons x y)))`. This says nothing about identity.
- These no longer tell the whole story, now that we have mutators.
- This raises a question: if you change the `car` of a pair, do all pairs that look the same also change?
- Pairs now have an identity.
- When we have multiple names for the same object, they are called _aliases_. Changing one changes them all. Sometimes sharing is what we want.

::: highlight
> But inadvertent sharing, unanticipated interactions between objects, is the source of most of the bugs that occur in complicated programs. So by introducing this possibility of things having identity and sharing and having multiple names for the same thing, we get a lot of power. But we're going to pay for it with lots of complexity and bugs. [@5b.p12]
:::

### Lambda calculus

- Assignment and mutators are equally powerful. We can implement the `cons` mutators in terms of `set!`.
- We've already seen Alonzo Church's way of creating pairs just with lambda expressions:

```
(define (cons x y)
  (lambda (m)
    (m x y)))
(define (car x) (x (lambda (a d) a)))
(define (cdr x) (x (lambda (a d) d)))
```

- We can change it to allow mutation:

```
(define (cons x y)
  (lambda (m)
    (m x
       y
       (lambda (n) (set! x n))
       (lambda (n) (set! y n)))))
(define (car x)
  (x (lambda (a d sa sd) a)))
(define (cdr x)
  (x (lambda (a d sa sd) d)))
(define (set-car! x v)
  (x (lambda (a d sa sd) (sa v))))
(define (set-cdr! x v)
  (x (lambda (a d sa sd) (sd v))))
```

- (I just realized that creating objects this way, where you apply the object to a procedure that is passed all the state variables, is just like pattern matching.)

# 6A: Streams, Part 1

## Part 1

### Recap

- We've learned about assignment and its frightening implications.
- The substitution model of evaluation breaks down; we have to use the much more complicated environment model.
- Worry about: assignment, state, change, time, identity, objects, sharing.
- Suddenly a variable doesn't just stand for a value; it specifies a place that holds a value.
- Our goal was _modularity_, writing programs that mirror reality.
- Maybe we have the wrong view of reality; maybe time is an illusion and nothing changes.

### Motivation

- We're going to look at another way to decompose systems: _stream processing_.
- Consider summing the odd squares in a binary tree of integers:

```
(define (sum-odd-squares tree)
  (if (leaf-node? tree)
      (if (odd? tree)
          (square tree)
          0)
      (+ (sum-odd-squares (left-branch tree))
         (sum-odd-squares (right-branch tree)))))
```

- And contrast with collecting the odd Fibonacci numbers:

```
(define (odd-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (odd? f)
              (cons f (next (1+ k)))
              (next (1+ k))))))
  (next 1))
```

- `sum-odd-squares`: enumerate leaves → filter `odd?` → map `square` → accumulate `+`, 0.
- `odd-fibs`: enumerate interval → map `fib` → filter `odd?` → accumulate `cons`, `'()`.
- These are similar, but the commonality is obscured by how we wrote the procedures.

::: highlight
> Going back to this fundamental principle of computer science that in order to control something, you need the name of it. [@6a.p4]
:::

### Defining streams

- The arrows between the boxes represent data structures called _streams_.
- `(cons-stream x y)` and `the-empty-stream` construct streams.
- For any `x` and `y`, `(head (cons-stream x y))` is `x`.
- For any `x` and `y`, `(tail (cons-stream x y))` is `y`.
- We can define higher-order procedures like we did for lists:

```
(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (proc (head s))
                   (map-stream proc (tail se)))))
```

### Using the language

- Now we can reimplement those problems with explicit stream processing:

```
(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd
                           (enumerate-tree tree)))))
(define (odd-fibs n)
  (accumulate cons
              '()
              (filter odd
                      (map fib
                           (enumerate-interval 1 n)))))
```

- This reveals their commonality, and makes it easy to mix and match boxes.
- The advantage of stream processing is that it establishes _conventional interfaces_.
- Conventional interfaces are powerful because we can easily glue things together.

## Part 2

### More complicated stream processing

- Suppose we have a stream of streams $\{\{1,2,3,\dots\},\{10,20,30,\dots\},\dots\}$.
- We can define a procedure `flatten` to accumulate them into a single flat stream.
- Problem: Given $N$, find all pairs $0<j<i≤N$ such that $i+j$ is prime.

```
(define (prime-sum-pairs n)
  (map
   (lambda (p)
     (list (car p) (cadr p) (+ (car p) (cadr p))))
   (filter
    (lambda (p)
      (prime (+ (car p) (cadr p))))
    (flatmap
     (lambda (i)
       (map
        (lambda (j) (list i j))
        (enumerate-interval 1 (-1+ i))))
     (enumerate-interval 1 n)))))
```

- `flatmap` takes the place of nested loops in most other languages.
- We can simplify it with syntactic sugar `collect`:

```
(define (prime-sum-pairs n)
  (collect
   (list i j (+ i j))
   ((i (enumerate-interval 1 n))
    (j (enumerate-interval 1 (-1+ i))))
   (prime? (+ i j))))
```

### Eight queens puzzle

- Solving this typically uses a _backtracking search_, navigating up and down the tree of possibilities until we get to the bottom (all queens placed).
- This is unnecessary -- it's inordinately concerned with _time_.
- A simpler way is to employ _wishful thinking_ and go from $k$ columns to $k+1$ columns.

```
(define (queens size)
  (define (fill-cols k)
    (if (= k 0)
        (singleton empty-board)
        (collect (adjoin-position try-row k rest-queens)
                 ((rest-queens (fill-cols (-1+ k)))
                  (try-row (enumerate-interval 1 size)))
                 (safe? try-row k rest-queens))))
  (fill-cols size))
```

- This gives us _all solutions_ to the $k$-queens puzzle.
- We changed our view from things that evolve in time and have state to a global view where we're not concerned with time.

## Part 3

### Streams are not lists

- By now you should be suspicious -- what's the catch?
- Problem: Find the second prime between 10,000 and 1,000,000.
- Stream solution: enumerate 10,000 to 1,000,000 → filter `prime?` → take 2nd.
- This is ridiculously inefficient. Our earlier programs (before streams) were ugly because they mixed all the operations up, but because of this they were efficient.
- But we can have our cake and eat it too! We can make it just as efficient.
- The key to this is that streams are _not_ lists.

### Implementing streams

- We want the stream to compute itself incrementally, to be an "on demand" data structure.
    - `(cons-stream x y)` is an abbreviation for `(cons x (delay y))`
    - `(head s)` is `(car s)`
    - `(tail s)` is `(force (cdr s))`
- `delay` creates a promise to compute something when `force`d.
    - `(delay «expr»)` is an abbreviation for `(lambda () «expr»)`
    - `(force p)` is `(p)`
- `delay` decouples the apparent order of events from the actual order of events that happen in the machine. We give up the idea that our procedures mirror some clear notion of time.
- One little hack: to be efficient, `(delay «expr»)` is `(memo-proc (lambda () «expr»))`.

```
(define (memo-proc proc)
  (let ((already-run? nil) (result nil))
    (lambda ()
      (if (not already-run?)
          (sequence (set! result (proc))
                    (set! already-run? (not nil))
                    result)
          result))))
```

- Streams blur the line between data structures and procedures.

# 6B: Streams, Part 2

## Part 1

### Recap

- We've been looking at stream processing, an on-demand method of computation.
- We don't compute elements until we've asked for them. When do we "ask" for them?
- Example: `nth-stream` forces the first `n` elements of a stream.

```
(define (nth-stream n s)
  (if (= n 0)
      (head s)
      (nth-stream (-1+ n) (tail s))))
```

### Infinite streams

- How long can a stream be? It can be _infinite_!

```
(define (integers-from n)
  (cons-stream n (integers-from (1+ n))))
(define integers (integers-from 1))
```

- Is this really all the integers, or is it just cleverly arranged so that whenever you look for an integer you find it there? This is a sort of philosophical question.

### Sieve of Eratosthenes

- Start with 2, cross out larger multiples of 2; take next, 3, and cross out larger multiples of 3; and so on. What you're left with is all the primes.

```
(define (sieve s)
  (cons-stream
   (head s)
   (sieve (filter (lambda (x) (not (divisible? x (head s))))
                  (tail s)))))
(define primes (sieve (integers-from 2)))
(nth-stream 20 primes)
=> 73
```

## Part 2

### Defining streams implicitly

- We've so far seen procedures that recursively create streams.
- There's another way. But first we need some additional procedures:

```
(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else (cons-stream (+ (head s1) (head s2))
                           (add-streams (tail s1) (tail s2))))))
(define (scale-stream c s)
  (map-stream (lambda (x) (* x c)) s))
```

- Now, we can define streams all at once:

```
(define ones (cons-stream 1 ones))
(define integers (add-streams integers ones))
```

### More examples

- We can calculate the integral $\int s\,dt$ the same way you would in signal processing:

```
(define (integral s initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream dt s) int)))
  int)
```

- Another example, the Fibonacci numbers:

```
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams fibs (tail fibs)))))
```

### Explicit `delay`

- Let's say we want to solve $y' = y^2,\; y(0) = 1$ using the step $dt = 0.001$.
- We'd like to write a stream program to solve this:

```
(define y (integral dy 1 0.001))
(define dy (map-stream square y))
```

- This doesn't work because `y` and `dy` each need the other defined first.
- We can fix it the same way `cons-stream` allows self-referencing definitions: by introducing another `delay`, so that we can get the first value of the integral stream without knowing what stream it's integrating.

```
(define (integral delayed-s initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((s (force delayed-s)))
                   (add-streams (scale-stream dt s) int))))
  int)
(define y (integral (delay dy) 1 0.001))
(define dy (map-stream square y))
```

## Part 3

### Normal-order evaluation

- We've been divorcing time in the program from time in the computer.
- Sometimes, to really take advantage of this method, you have to write explicit `delay`s. But in larger programs it can be very difficult to see where you need them.
- Is there a way around this? Yes, by making _all_ arguments to _every_ procedure delayed.
- This is _normal-order_ evaluation, as opposed to _applicative-order_ which we've been using.
- We wouldn't need `cons-stream` because it would be the same as `cons`.
- But there's a price. The language becomes more elegant, but less expressive. For example, we could no longer write iterative procedures.
- You get these "dragging tails" of thunks that haven't been evaluated, taking up 3 MB (!).
- A more striking issue: normal-order evaluation and side effects just don't mix.
- The whole idea with streams was to throw away time; but with side effects we want time!
- _Functional_ programming languages avoid this issue by disallowing side effects.

### Avoiding mutable state

- What about generating random numbers? We wanted modularity, so we encapsulated the random number generation with local state.
- Instead, we could create an infinite stream of random numbers.
- With bank accounts, instead of using local state and message passing, we can have the bank account process a stream of transaction requests, and emit a stream of balances.
- Can you do everything without assignment? No, there seem to be places where purely functional programming languages break down.
- For example, how do we model a joint bank account with multiple users? We'd have to merge the request streams somehow.
- The conflict between objects/state/time and `delay`/streams/functional programming might have very little to do with CS, and be more about different ways of viewing the world.

# 7A: Metacircular Evaluator, Part 1

## Part 1

## Part 2

## Part 3

# 7B: Metacircular Evaluator, Part 2

## Part 1

## Part 2

## Part 3

# 8A: Logic Programming, Part 1

# 8B: Logic Programming, Part 2

# 9A: Register Machines

# 9B: Explicit-Control Evaluator

# 10A: Compilation

# 10B: Storage Allocation and Garbage Collection
