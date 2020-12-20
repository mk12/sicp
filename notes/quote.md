---
title: Quotes
quote: true
---

# Textbook

Page numbers correspond to the [2.andresraba5 PDF][pdf].

## Preface

> First, we want to establish the idea that a computer language is not just a way of getting a computer to perform operations but rather that it is a novel formal medium for expressing ideas about methodology. Thus, programs must be written for people to read, and only incidentally for machines to execute. (xxii)

> Underlying our approach to this subject is our conviction that "computer science" is not a science and that its significance has little to do with computers. (xxiii)

> Mathematics provides a framework for dealing precisely with notions of "what is." Computation provides a framework for dealing precisely with notions of "how to." (xxiii)

## Chapter 1: Building Abstractions with Procedures

> Syntactic sugar causes cancer of the semicolon. (Alan Perlis)

> In testing primality of very large numbers chosen at random, the chance of stumbling upon a value that fools the Fermat test is less than the chance that cosmic radiation will cause the computer to make an error in carrying out a "correct" algorithm. Considering an algorithm to be inadequate for the first reason but not for the second illustrates the difference between mathematics and engineering. (69)

## Chapter 2: Building Abstractions with Data

> Developing a useful, general framework for expressing the relations among different types of entities (what philosophers call "ontology") seems intractably difficult. The main difference between the confusion that existed ten years ago and the confusion that exists now is that now a variety of inadequate ontological theories have been embodied in a plethora of correspondingly inadequate programming languages. For example, much of the complexity of object-oriented programming languages -- and the subtle and confusing differences among contemporary object-oriented languages -- centers on the treatment of generic operations on interrelated types. (270)

## Chapter 3: Modularity, Objects, and State

> In general, programming with assignment forces us to carefully consider the relative orders of the assignments to make sure that each statement is using the correct version of the variables that have been changed. This issue simply does not arise in functional programs. (318--319)

> In view of this, it is ironic that introductory programming is most often taught in a highly imperative style. This may be a vestige of a belief, common throughout the 1960s and 1970s, that programs that call procedures must inherently be less efficient than programs that perform assignments. [...] Alternatively it may reflect a view that step-by-step assignment is easier for beginners to visualize than procedure call. Whatever the reason, it often saddles beginning programmers with "should I set this variable before or after that one" concerns that can complicate programming and obscure the important ideas. (319)

> The truth of the matter is that, in a language in which we can deal with procedures as objects, there is no fundamental difference between "procedures" and "data," and we can choose our syntactic sugar to allow us to program in whatever style we choose. (378)

> To quote some graffiti seen on a Cambridge building wall: "Time is a device that was invented to keep everything from happening at once." (403)

> We can model the world as a collection of separate, time-bound, interacting objects with state, or we can model the world as a single, timeless, stateless unity. Each view has powerful advantages, but neither view alone is completely satisfactory. A grand unification has yet to emerge. (486)

> The object model approximates the world by dividing it into separate pieces. The functional model does not modularize along object boundaries. The object model is useful when the unshared state of the "objects" is much larger than the state that they share. (486)

## Chapter 4: Metalinguistic Abstraction

> Establishing new languages is a powerful strategy for controlling complexity in engineering design; we can often enhance our ability to deal with a complex problem by adopting a new language that enables us to describe (and hence to think about) the problem in a different way, using [terms] that are particularly well suited to the problem at hand. (488)

> It is no exaggeration to regard this as the most fundamental idea in programming: The evaluator, which determines the meaning of expressions in a programming language, is just another program. (489)

> In a similar way, we can regard the evaluator as a very special machine that takes as input a description of a machine. Given this input, the evaluator configures itself to emulate the machine described. [...]
>
> From this perspective, our evaluator is seen to be a _universal machine_. It mimics other machines when these are described as Lisp programs. This is striking. Try to imagine an analogous evaluator for electrical circuits. This would be a circuit that takes as input a signal encoding the plans for some other circuit, such as a filter. Given this input, the circuit evaluator would then behave like a filter with the same description. Such a universal electrical circuit is almost unimaginably complex. It is remarkable that the program evaluator is a rather simple. (522--524)

> The word thunk was invented by an informal working group that was discussing the implementation of call-by-name in Algol 60. They observed that most of the analysis of ("thinking about") the expression could be done at compile time; thus, at run time, the expression would already have been "thunk" about. (545)

# Lectures

Quotes and paraphrases are from the [MIT OCW video lectures][lectures].

## 1A: Overview and Introduction to Lisp

> Computer scientists are in the business of controlling complexity.

## 1B: Procedures and Processes; Substitution Model

> But one of the things he have to learn how to do is to ignore details. The key to understanding complex things is knowing what not to look at, and what not to compute, and what not to think.

## 2B: Compound Data

> See, in general, as systems designers, you're forced with the necessity to make decisions about how you're going to do things. And in general, the way to retain flexibility is to never make up your mind about anything until you're forced to do it. The problem is, there's a very, very narrow line between deferring decisions and outright procrastination. So you'd like to make progress, but also at the same time never be bound by the consequences of your decisions.

> I said that computer science is a lot like magic, and it's sort of good that it's like magic. There's a bad part of computer since that's a lot like religion.

## 3A: Henderson Escher Example

> Lisp is a lousy language for any particular problem. But it's good for figuring out the right language and embedding that in Lisp. That's the real power of this approach to design.

> The design process is not so much implementing programs as implementing languages. That's the powerful idea of Lisp.

## 3B: Symbolic Differentiation; Quotation

> In order to make a robust system, it needs to be insensitive to small changes. A small change in the problem leads to a small change in the solution. Don't solve a particular problem at each level; solve a class of problems in the neighbourhood of the particular problem by building a language suited to them.

## 4A: Pattern Matching and Rule-based Substitution

> The key to very good programming, and very good design, is to know what not to think about. 

## 5A: Assignment, State, and Side-effects

> We like to think about the world as being made up of independent objects, each having their own state. The idea of objects, changed, and sameness raises deep problems. If you cut off your fingernail, the "before" and "after" of you is enormously different in terms of atoms. You have changed and feel that you are the "same" person, but how do you know that? By introducing assignment and therefore objects, we open ourselves up to all of these philosophical questions.

## 5B: Computational Objects

> When we have multiple names for the same object, they are called aliases. Changing one changes them all. Sometimes sharing is what we want. But inadvertent sharing is a great source of bugs in complicated programs.

## 6A: Streams, Part 1

> Going back to this fundamental principle of computer science that in order to control something you need the name of it.

[sicp]: https://mitpress.mit.edu/sites/default/files/sicp/index.html
[pdf]: https://github.com/sarabander/sicp-pdf
[lectures]: https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/
