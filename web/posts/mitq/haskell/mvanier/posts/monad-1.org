---
title: Yet Another Monad Tutorial (part 1":" basics)
tags: mvanier, monads, tutorials, haskell
date: 2010-07-25
author: Mike Vanier
description: Mike Vanier's article about moands - part 1
comments: true
toc: true
---
[original article](http://mvanier.livejournal.com/3917.html)


It's a standing joke in the Haskell community that every Haskell programmer, as part of his or her learning process, will eventually write one or more monad tutorials. I'm certainly no exception. But since I know that there are already dozens of monad tutorials out there, some quite good, why on earth would I want to write Yet Another One? There are two reasons:

* I think I can explain some aspects of monads better than most of the monad tutorials I've seen.

* My own understanding of monads has improved greatly, and I'd like to try to pass that on if I can.

## Prerequisites

Since I will be using the programming language **Haskell** in my examples, it would be very helpful if you, the reader, know Haskell up to and including

* polymorphic types
* type classes

If you don't, you may find the material in these articles to be hard to understand. There are lots of good introductory Haskell tutorials on the web and in print, so you might want to come back to this series of articles after reading one of them.

One prerequisite I will not be assuming is any knowledge of category theory, which is a very abstract branch of mathematics where the concept of a monad (as the term is used here) originated. Although knowing category theory is not harmful, it is definitely not necessary in order to understand the material here, so don't believe anyone who tells you that you need to know category theory before you can understand monads in the context of programming languages — you don't. If you do know category theory, then good for you, but realize that I will be making no effort to use category-theoretic terminology here (other than the word "monad").

## Disclaimer

I'm not going to teach you everything there is to know about monads in these articles, for two reasons: **one**, it would take way too long, and **two**, I don't know everything there is to know about monads and probably never will. Instead, I want to give you **a clear conceptual understanding of**

1. what monads are,
2. why they are useful,
3. the essentials of how they work,
4. what some of the most common monads in use are.

I'll give some references for further study at the end of the [last article](moand_8.html) in the series.

I'm also not going to be giving you tons of practical code samples you can immediately use in your day-to-day programming work. This is not a cookbook! I believe strongly that you need to really understand what you're doing when you program, and this tutorial is designed to give you a detailed understanding of monads. With it, you can then read any of a number of other monad tutorials (again, see the references) to get a better idea of how monads are used to solve practical programming problems. But the point here is to give you the *big picture* and help you really understand **what monads are** and **how they work**.

Finally, I warn you in advance that I am going to belabor the hell out of my main points, repeating them over and over and basically beating them to death, because I really want you to understand what I'm trying to say. I hope it won't be boring, but it will be long, because you can't explain monads in a few sentences. So brew up a pot of coffee and make sure you've got a comfy chair to sit in, because this will take a while.

## Motivation: Why should you care about monads?

To the best of my knowledge, the first use of monads in programming languages was in Haskell, based on <a target="_blank" href="http://www.disi.unige.it/person/MoggiE/ftp/ic91.pdf">the work</a> a of Eugenio Moggi and Philip Wadler (two giants whose shoes I am not fit to shine). Since then, they have appeared in some other programming languages (especially functional languages). But why should you, the reader (who I'm assuming is a programmer but who may not yet have drunk the functional kool-aid) care about monads?

### Pure Functions
One of the main ideas in functional programming is **to use _pure_ functions as much as possible.** A pure function is a black box: it takes its input argument(s) and computes and returns a value, and that's it. It does not do any kind of side-effect in the process:

* no reading or writing to files or sockets,
* no printing to terminals,
* no reading or altering global variables,
* no throwing or catching exceptions,
* etc.

The benefit of this is that pure functions are well-behaved: *if you give a pure function a particular input, it will always generate the exact same output.* This makes pure functions very easy to:

1. test,
2. completely predictable,
3. less prone to bugs.

In contrast, impure functions (those that have side-effects) do not necessarily compute the same answer given the same inputs (for instance, they may give a different answer if a global variable that they use has a different value, or if a file that they are reading from has different contents). Impure functions are therefore harder to test, more prone to bugs, and there are more ways in which they can fail. For this reason (and for others we'll see later), functional programming languages emphasize the use of pure functions as much as possible.


### Side Effects

And yet, programming with only pure functions is too limiting. In some cases, side effects make certain programs much easier to write, though they still could be written (painfully) with pure functions only. In other cases, you absolutely need the ability to do side effects; without this the programs just can't be written. For instance, a program that copies a file from one part of a directory structure to another necessarily has to interact with the file system and change it; if your functions are not allowed to read or write files (which are side effects) they won't be able to do it. So we need some way to do side effecting computations, even in functional languages.

There are two classes of functional languages: pure and impure. Impure functional languages (like Scheme or Ocaml) basically punt on this problem: they allow you to write functions which have side effects, even though users of impure languages usually try to avoid doing so unless absolutely necessary. Pure functional languages (like Haskell) are more hard-core: they don't allow you to directly write side-effecting functions at all (you'll see why I say directly below). Therefore, as you might imagine, figuring out a way to write side-effecting programs in pure functional languages was a major research topic for a long time.

### Monads
Monads turned out to be the key to solving this problem. (More precisely, they are a key; some other functional languages use different approaches, like <a target="_blank" href="http://clean.cs.ru.nl/Clean">Clean</a>'s *uniqueness types.*) Monads allow us to do all the side-effecting computations we want in a pure functional language, but without destroying the purity of the language. With monads we can use the type system to cleanly separate out side-effecting computations from ones that don't have side effects so that neither kind of computation interferes with the other. So we get all the benefits of functional programming for code that doesn't do side-effects (and the type system guarantees that those functions don't do side effects) while still being able to do side effects when necessary. This is extremely powerful.

And as if that wasn't enough, monads turn out to have lots of other uses as well. They are actually a very general tool for structuring various kinds of computations in a well-behaved way, and they can drastically simplify many different kinds of programs — not just ones that involve side-effects. In many cases, monadic code can be vastly shorter and more comprehensible than equivalent non-monadic code would be; we'll see examples of this as we proceed. So monads have an applicability that goes way beyond helping us deal with side-effects in functional languages (though they give us that too).

Monads are truly one of the most amazing ideas in programming languages, and are well worth learning.

# Executive summary: What are monads?

**"What is a monad?"** is a question I've been asked many times. I don't want to describe a monad as a **"thing"** because that is uninformative and also misleading. Instead, my executive summary is this:

> **Monads are a generalization of functions, function application, and function composition   to allow them to deal with richer notions of computation than standard functions.**

As we progress, I hope to explain to you not only what monads are and how they work, but also why monads can be so confusing to programmers unfamiliar with them. (Hint: it isn't because they're not smart enough or because they don't know enough category theory.)

## Notions of computation

OK, so let's begin the task of breaking down my executive summary by looking at what is meant by a "notion of computation".

The simplest and best-behaved "notion of computation" is ordinary (pure) functions (*i.e.* the mathematical definition of functions). For simplicity, I'll only consider functions mapping a single input argument to a single output. (It's possible to reduce multi-argument functions to single-argument functions by a process known as [currying](#aside-currying), and I'll have more to say about that below. For now, just take my word for it.) As I said above, a pure function is just a rule which for a particular input will always generate the exact same output. In strongly-typed languages like Haskell, a function has a well-defined type signature, which means that there are types ```a``` and ```b``` such that the function maps a value of type ```a``` to a value of type ```b```. We can express this in Haskell notation as follows:

``` haskell
f :: a -> b
```
where the "```::```" means "has the following type". So the function ```f``` has the functional type ```a -> b```, which means that it takes in a value of type ```a``` and returns a value of type ```b```. In practice, ```a``` and ```b``` will usually be the names of specific actual types (like ```Int```, or ```Float```, or ```String```) but in some cases Haskell functions can work the same regardless of the type of the argument.

So pure functions are the simplest "notion of computation". What are some others? There are lots of them that are familiar to programmers; they would include computations that (in addition to mapping an input to an output),

* may do file or terminal input/output
* may raise exceptions
* may read or write shared state (global or local)
* may sometimes fail to produce any results
* may produce multiple results at the same time

and many more. Note: from now on, I'll use the phrase "input/output" (or "I/O" for short) to refer to file or terminal input/output (also known as side-effecting input/output); don't confuse this with the fact that a function maps an input *value* to a specific output *value*.

### In imperative language
Think for a second about how you might want to deal with these alternative notions of computation in a conventional programming language like **C** or **Java.**

* **I/O** - Computations that may do I/O. No problem! Any function in C or method in Java can do I/O.

* **Exceptions** -
How about computations that may raise exceptions? In C this is a bit tricky, because the language doesn't have built-in support for exceptions. What you usually do is return an error code in the case of failure which specifies something about the failure condition (or you can use ```setjmp/longjmp``` if you're really hard-core). In Java you can just raise an exception and be done with it (hopefully, the exception will be caught somewhere else).

* **Shared State** -
How about reading and writing shared state? No problem — both C and Java let you read and write local and global variables, though the details are naturally somewhat different.

* **Computation that may Fail** -
Computations that may fail? These can be treated as a degenerate case of functions that can raise exceptions, so there's no problem there either.

* **Nondeterminacy (computations with multiple results)** -
How about computations that can produce multiple results? Actually, by this I don't mean returning multiple results as a single object (e.g.. in a **C** struct or a **Java** object); I mean functions that can return multiple single results "in parallel" (also known as **nondeterminacy**). It's not at all clear how to do this in **C** or **Java**.

**The important thing to note is this:** in each case, we are no longer dealing with the traditional notion of function, since "something else" is happening along with the usual functional effect of mapping an input to a single output. Furthermore, there are multiple different kinds of "something else-ness" represented by all these different notions of computation. We usually don't think much about this when we write programs; we just accept that the functions that we are writing aren't "really" the same as the mathematical notion of function, because they can do **I/O**, raise **exceptions**, alter the state of **global variables**, etc. This doesn't bother most programmers until they run into a nasty bug that is caused by an unexpected change in a global variable, or an unexpected exception being thrown, or some other problem that relates to the non-function-ness of functions in these languages. So we'd like to use pure functions as much as possible, except that (as I mentioned above) there are many cases where this isn't practical, and we really have to do the "something else" *i.e.* the computations that have side-effects.

The upshot of this is: we would like to be able to have our cake and eat it too. We would like to write our programs using pure functions as much as possible, with all the benefits that this provides (easy debuggability, easy composability). But we would also like to be able to do those "something elses" in a *controlled* way when doing so would be necessary or just advantageous. And that's what monads are going to give us.

**BUT:** the key phrase in the last paragraph is "in a *controlled* way". If everything worked the way it did in **C** or **Java**, we could do what we wanted for many (but not all) non-pure-functional notions of computation, but we would lose all the benefits of functional programming, because we would have no assurance that any of our program's functions were pure functions (and the type checker couldn't help us do this). So we need a systematic way of dealing with these other notions of computation that doesn't pollute the code that doesn't involve them (the purely functional code).

At this point, it will help to review the basic notions of (pure) functions, (pure) function application, and (pure) function composition, so that we will be able to contrast this with the monadic way of doing similar things.

## Functions, function application and function composition

### Functions
As I mentioned above, functions in Haskell use a particular notation to specify the types of their inputs and outputs. For a function ```f``` that has an input type ```a``` and an output type ```b```, this notation is:

``` haskell
  f :: a -> b
```

So ```f``` has type ```a -> b``` (pronounced "```a``` arrow ```b```" or just "```a``` to ```b```"). To give a more specific example, here is the definition of a function that doubles its input:

``` haskell
  f :: Int -> Int
  f x = 2 * x
```

```f``` has type ```Int -> Int``` because it takes in an integer, multiplies it by 2, and returns another integer.

### Function application
To do something with a function, we have to apply it to its argument (we're assuming one-argument functions here). This is usually done by simply juxtaposing the function name with the argument:
``` haskell
  f 2  --> has value 4
```
Note that in Haskell, unlike most computer languages, we don't have to surround a function's arguments with parentheses([see currying](#aside-currying)).

#### ```$``` operator
There is also an explicit operator called ```$```which is the function application operator. It has the type:
``` haskell
  ($) :: (a -> b) -> a -> b
```

[In Haskell, symbolic infix operators are equivalent to functions with the same name surrounded by parentheses, so ```f $ 2``` means the same thing as ```($) f 2```. When defining new symbolic operators we often write them in their functional form for convenience (see any introductory Haskell tutorial for more on how to do this). We will be using this facility a lot below.]

This means that, for any types ```a``` and ```b```, this operator takes a function from type ```a``` to type ```b``` (its first argument), applies it to an argument of type ```a``` (the second argument) and returns a value of type ```b```. In a functional language, it's legal to pass functions as arguments to other functions, so there is no problem with this. So:
``` haskell
  f 2      --> has value 4
  f $ 2    --> also has value 4
  ($) f 2  --> also has value 4
```
These are just three different ways of writing the exact same thing.

Now, using the ```$``` operator for function application isn't technically necessary because you can just juxtapose the function with its argument to apply the function to the argument (though there are actually some common uses for ```$``` involving operator precedence that we won't get into here).

#### ```>$>``` operator ("reverse apply")
Interestingly, we can also define a "reverse apply" operator (which we'll call ```>$>```) that is like ```$``` but takes its arguments in the reverse order:
``` haskell
  (>$>) :: a -> (a -> b) -> b
  x >$> f = f x  -- = f $ x as well
```

This is somewhat appealing in that we can read it as "take a value x, apply the function f to it, and get the result". If you know unix, you may notice that the unix shell's pipe (```|```) operator works this way — you produce some data and then apply a program to it to transform it in some way. We can use whichever function application operator is more convenient for our purposes at any given time, though usually we don't use an operator at all, just juxtaposition[^juxtaposition].

### Function composition

Now that we've talked about function application, the next important topic is function composition, and it's really important. Let's say we have two functions ```f``` and ```g``` and a value ```x``` with the following types:

``` haskell
  x :: a
  f :: a -> b
  g :: b -> c
```
for some types ```a```, ```b```, and ```c```. One thing you might want to do with ```x```, ```f```, and ```g``` is to take the value ```x```, pass it to the function ```f``` (which converts the value ```x``` (of type ```a```) into a value that has type ```b```), and then pass that value (of type ```b```) to the function ```g``` (which converts the value of type ```b``` into a value of type ```c```). The way to write this in Haskell is:

``` haskell
  g (f x)
```

Note that this will only work if the types of ```f``` and ```g``` are compatible, *i.e.* if the type of the output of ```f``` is also the type of the input of ```g``` (here, type ```b```). A different way of looking at this is that we are really taking the two functions ```f``` and ```g``` (of types ```a -> b``` and ```b -> c``` respectively), combining them into a function of type ```a -> c```, and applying that function to ```x``` (type ```a```) to get a value of type ```c```.

#### ```.``` operator
This idea of taking two functions and generating a third one from them is called function composition, and it's very easy to define an operator for function composition (called ```.``` in Haskell):

``` haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \x -> g (f x)
```
Here, we're using the notation ```\x -> ...``` to mean a lambda expression (anonymous function) of one argument ```x```. So the function composition operator ```.``` takes two functions as its arguments and returns a single function. Again, in functional languages this kind of function is perfectly valid because functions are acceptable as arguments of, or as return values from, other functions.

#### ```>.>``` operator ("reverse function composition")
One thing about the ```.``` operator that is a bit ugly is that the arguments are not in the most obvious order. We can write a "reverse function composition" operator (which I'll call ```>.>```) as follows:

``` haskell
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
f >.> g = \x -> g (f x)
```

We could also define it using the ```>$>``` operator defined above as:

``` haskell
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
f >.> g = \x -> x >$> f >$> g
```

Or, even more simply, as:

``` haskell
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
f >.> g = g . f
```

The ```>.>``` operator has a type signature that makes it clearer what's really happening with function composition. You take functions ```f``` and ```g``` and compute a new function (call it ```h```). Applying ```h``` to a value is equivalent to first applying ```f``` to the value and then applying ```g``` to the result. That's all function composition is: a way to take existing functions and make new functions from them.

Here's an example:

``` haskell
f :: Int -> Int
f x = 2 * x

g :: Int -> Int
g y = 3 + y

h :: Int -> Int
h = g . f  -- or equivalently: f >.> g

```

What does ```h``` do here? It takes in an integer, multiplies it by 2, and adds 3 to it. So it's equivalent to:

``` haskell
h :: Int -> Int
h x = 3 + 2 * x
```

Function composition may not seem like a big deal, but it's one of the keys to functional programming. It allows us to take existing functions and easily "snap them together" to form more complex functions without having to write all the arguments out by hand. So instead of saying, in effect, "```h(x)``` is the function we get when we first compute ```f(x)``` to give us ```y```, and then compute ```g(y)``` to give us ```z```, and then return ```z```" we are saying "```h``` is the function we get by applying ```f``` and then ```g```". Not having to name the intermediate values makes code more concise and high-level. Imagine if you were composing ten functions together one after another — if you had to write out all the intermediate results, it would look something like this (we'll assume all the functions have type ```Int -> Int```):

``` haskell
 f11 x =
     let
       x2 = f1 x
       x3 = f2 x2
       x4 = f3 x3
       x5 = f4 x4
       x6 = f5 x5
       x7 = f6 x6
       x8 = f7 x7
       x9 = f8 x8
       x10 = f9 x9
       x11 = f10 x10
     in
       x11
```

Pretty tedious, right? Now look at what we get when we use function composition:

``` haskell
f11 = f10 . f9 . f8 . f7 . f6 . f5 . f4 . f3 . f2 . f1
```

or, equivalently:

``` haskell
f11 = f1 >.> f2 >.> f3 >.> f4 >.> f5 >.> f6 >.> f7 >.> f8 >.> f9 >.> f10
```
#### point-free style = argument-free style
This is not only shorter but more intuitive ("```f11``` is what you get when you first apply ```f1```, then ```f2```, then ```f3``` ..."). In fact, this way of writing functions using composition and without specifying the values that the functions act on is called "point-free style". (This name is extremely ironic given that the ```.``` (point) operator is actually used more in point-free code than in regular code — the word "point" in "point-free" really means "argument" so perhaps "argument-free style" would be a better name.)

The take-home lessons from this section are:

* Functions, function application, and function composition are fundamental concepts in functional programming.

* We can write operators for function application and function composition, and these operators can take their arguments in whatever order we want.

## Monadic functions, monadic values

So far, everything I've said has (I hope) been pretty straightforward. Now we're going to get into the more complicated stuff.

I've said above that the point of monads is to generalize the notions of function application and function composition to notions of computation which are different from pure functions, and I've talked about what some of these notions are.

#### Extended function
If we could write this down schematically in a pseudo-Haskell notation, we might want to write the type of one of these "extended functions" (functions that do something else besides take in an input value and compute and return an output value) as something like this:

``` haskell
f :: a --[something else]--> b
```

for extended function ```f```, input type ```a``` and output type ```b```. The "something else" is specific to a particular notion of computation. In Haskell, special "notions of computation" correspond to particular monads (we don't know what a monad is yet, so trust me on this for now), and we can refer to these extended functions as "monadic functions" (this isn't standard terminology; it's just my way of distinguishing these extended functions from pure functions).

Note that this notation with the ```--[something else]-->``` is not legal Haskell syntax; we'll see how Haskell handles this below, and it will hopefully be illuminating. But for now, we'll stick with this notation and look at the different notions of computation we described above, giving the name that Haskell assigns to each one where feasible:

1. Functions that may do (terminal or file) input/output. This corresponds to the ```IO``` monad, so we could write this as

``` haskell
  f :: a --[IO]--> b

```
    [In fact, the ```IO``` monad also has other uses which we'll see later.]

2. Functions that may raise exceptions. This correspond to various kinds of error monads:

``` haskell
  f :: a --[error]--> b
```

3. Functions that can interact with global or local state. This corresponds to the ```State``` monad:

``` haskell
  f :: a --[State s]--> b
```
The ```s``` in (```State s```) is the type of the state that is being manipulated.

4. Functions that can fail. This corresponds to the ```Maybe``` monad:

``` haskell
  f :: a --[Maybe]--> b
```

5. Functions that can return multiple values in parallel. This corresponds to the ```list``` monad:
I didn't capitalize "list" because lists have special syntactic support in Haskell, so we don't need to define a special name for them.

``` haskell
  f :: a --[list]--> b
```

#### Example of I/O

I'll give examples of all of these monads later in the series. For now, let's consider functions that may do terminal or file input/output i.e. are in the ```IO``` monad. We have:

``` haskell
  f :: a --[IO]--> b
```
and so we could say that ```f``` is a function from ```a``` to ```b``` in the ```IO``` monad. As I said above, this is not legal Haskell syntax. In Haskell, you have to stuff the "monad-ness" of a monadic function into a type; in this case, you have to put it into either the input type or the output type. So, in principle, we might imagine that we could change our monadic function to have one of the following two type signatures:

``` haskell
  f :: IO a -> b
```
or
``` haskell
  f :: a -> IO b
```

It turns out that in Haskell, monadic functions always have the second form:

``` haskell
  f :: a -> m b
```
or some monad ```m``` (in this case, ```m``` is ```IO```). (Side note for the truly hard-core: there is a related notion called a "comonad" which uses functions like ```f :: c a -> b``` for some comonad ```c```. I'll leave that for a later article.)

OK, then, what the heck does "```f :: a -> m b```" really mean? It means that ```f``` is a regular (pure) function which takes input values of type ```a```, and returns output values of type ```m b``` (whatever they are). So in Haskell, these monadic functions are represented as pure functions with a funky "monadic return type"; put differently, the pure function takes in regular values and returns funky "monadic values". But what does that mean?

The notation "```m b```" needs explanation. ```b``` is some Haskell type, and ```m``` will represent some kind of monad. But what is ```m``` as far as Haskell is concerned? In Haskell, ```m``` has to be a type constructor, which is basically a function on types: it takes a type argument and returns a type. This is less weird than it may seem. Consider the notion of "list of Int" (written in Haskell as type ```[Int]```). The "list of" part can be thought of as a type constructor that takes a particular type (```Int```) and turns it into another type (```list of Int```, or ```[Int]```). The square bracket notation for list types is hard-coded into Haskell, but it's perfectly possible to define your own type constructors. In fact, any polymorphic type in Haskell has a corresponding type constructor. One of the simplest is the ```Maybe``` polymorphic type, defined as:

``` haskell
data Maybe a = Nothing | Just a
```

What this says is that ```Maybe``` is a type constructor which takes as input one type (called ```a```) and produces a type as output. If ```a``` is ```Int```, then the resulting type is ```Maybe Int```, and it's just as if we'd written:

``` haskell
  data Maybe Int = Nothing | Just Int
```
So ```Maybe``` itself is a function on types mapping one input type to one output type. There is a technical name for this: the type constructor ```Maybe``` has the kind   ```* -> *```. A "kind" is a type of types; primitive types have the kind *, which just means that they aren't type functions (i.e. type constructors). Don't worry if this seems confusing; it isn't particularly important in what follows.

What is important is that monads, as represented in Haskell, are type constructors like this, turning an input type into a new type. So the ```IO``` monad described above is in fact a type constructor, and there are types like ```IO Bool```, ```IO Int```, ```IO Float```, ```IO Char```, ```IO String``` etc. which all represent valid Haskell types. Similarly, it will turn out that ```Maybe``` will be a monad, and types like ```Maybe Bool```, ```Maybe Int``` etc. are all valid Haskell types. I will refer to types that are made from a monadic type constructor to be "monadic types", so ```IO Bool```, ```Maybe Int```, etc. are all monadic types.

> *Side note:* although all monads in Haskell are type constructors, not all type constructors are monads. As we will see, monads have to be type constructors for which specific operations are defined and for which specific "monad laws" hold.

Now we get to a very big question: what do values that happen to have monadic types represent? I call these "monadic values". What does a value of type ```Maybe Int``` represent? What does a value of type ```IO Float``` represent?

We have just hit on the crux of why monads are "hard to understand".

## Let's recap:
1. There is a familiar notion of "pure function" *i.e.* a function which does nothing more than convert an input value of some type into an output value of a (possibly different) type.

2. There are some special kinds of functions that do something else besides just converting input values into output values. That "something else" can be doing
    * terminal or file input/output,
    * raising exceptions,
    * interacting with global or local state,
    * possibly failing,
    * possibly returning more than one result,
    * or other things.

    Each of these special kinds of functions corresponds to a particular monad, and I refer to them as "monadic functions". The notion of a monadic function should be fairly intuitive, as every programmer uses functions like this all the time (but without calling them "monadic functions").

3. Haskell represents monadic functions as pure functions which convert an input value of some type into an output value of a special monadic type. I refer to these output values as "monadic values".

Now let's restate the problem: what do "monadic values" really represent?

Here's the answer: **They don't really represent *anything* intuitive!** The intuitive concept is the notion of a monadic *function* (*i.e.* a function which does something else besides convert its input data into some output data). The concept of a monadic *value* is not intuitive; it's just how Haskell has chosen to represent the outputs of monadic functions. So if you've been trying to understand Haskell monads by understanding what monadic values "really mean", you have been wasting your time! Don't bother! It isn't necessary!

Nevertheless, In the Haskell literature, there are two common ways of trying to explain what a monadic value is (along with a bunch of silly ways that are occasionally used, mainly in tutorials):

1. A monadic value of type ```m a``` (for some monad ```m```) is some kind of "action" that does something (the exact nature of which depends on the specific monad) and "returns" a value of type ```a```.

2. A monadic value of type ```m a``` (for some monad ```m```) is kind of like a container that stores values of type ```a```.

So even though thinking of monadic values is the wrong way to approach monads (and thinking of monadic functions is the right way), I want to try to convince you that (1) at least makes some sense. As we'll see later, (2) is the wrong way to think about monads; most monad aren't containers, though some particular monadic values can also behave as containers.

Let's take our (hopefully fairly intuitive) notion of a monadic function as our starting point:
``` haskell
  f :: a -> m b
```

Then ```f x```, where ```x``` has type ```a```, would have the type ```m b```:
``` haskell
  x :: a
  f x :: m b
```

Now ```f x``` is a "monadic value", which isn't very intuitive. Let's consider a new function:

``` haskell
  g :: a -> () -> a
  g x () = x
```
What ```g``` does is take a value (of any type ```a```) and wrap it into a function so that you can only retrieve the value by calling the function with a unit value. The ```unit type``` and ```value``` are both written as ```()``` in Haskell, and it's just a type/value that has no significance (the name "unit" just means that it's a type that has only one value, so the value can't mean anything in particular). So, for instance, we could have

``` haskell
 h = g 10
 h ()   -- this will evaluate to 10
```

So what is ```g (f x)```? It has the type:

``` haskell
  f x :: m b  -- see above
  g :: a -> () -> a
  g (f x) :: () -> m b
```

So ```g (f x)``` has the type ```() -> m b```. In other words, it's a function which takes a unit value as its argument and returns a monadic value. But looked at another way, it's a monadic function which converts a unit value (a value of no significance) into a value of type ```b```, also doing "something else" in the process (the "something else" depending on the monad). This should make sense.

Now here's my point. If you feel the need to understand what a monadic value (of type ```m b```) really means, the closest you can get is that it's like a monadic function of type ```() -> m b``` *i.e.* a function which maps the ```unit value``` to a value of type ```b```, doing something else in the process. So it's as if values of type ```m b``` are really functions of type ```() -> m b```, except that they're not written that way. Monadic values are "undercover functions" as it were. That's why they're often called "actions", which connotes something like a function, but not quite. (Sometimes we also talk about "performing an action" or "executing an action" which is like applying the function.)

At this point, a couple of examples won't kill us. I'll use the example of two input/output functions in Haskell:

``` haskell
  getLine  :: IO String
  putStrLn :: String -> IO ()
```

```getLine``` is a "function" (really a monadic value AKA a monadic "action") which reads a line of text from the terminal, somehow returning the line of text read as a string. ```putStrLn``` is a function (a real function this time) which takes a string as input and displays it on the terminal, also outputting a newline character at the end.

Think for a second how the types of these two functions would be written in a more conventional language. They would probably look something like this:

``` haskell
getLine  :: () -> String   -- not in Haskell
putStrLn :: String -> ()   -- not in Haskell
```

This should be easy to understand: ```getLine``` takes a unit value (of no significance) as its input, interacts with the terminal somehow (getting a line of text in the process), and returns a string. ```putStrLn``` takes a string as its input, interacts with the terminal somehow (printing the string onto the terminal in the process), and returns a ```unit value``` (of no significance). Notice in this case that the purpose of the unit values in these two functions are simply to make sure that they are real functions *i.e.* that they have an input and an output value. If we removed the ```()```s from the type signatures of ```getLine``` and ```putStrLn```, we'd be left with:

``` haskell
 getLine  :: String
 putStrLn :: String
```
which is clearly wrong — ```getLine``` isn't just a string; it's a function that has to be called with an argument and which returns a string. Similarly, ```putStrLn``` isn't a string, it's a function which takes a string as an argument and returns an unimportant value. In both cases, the unit types are there as placeholders to make sure that the functions have both inputs and outputs.

Let's go back to Haskell. We have:

``` haskell
  getLine  :: IO String
  putStrLn :: String -> IO ()
```
The type of ```putStrLn``` isn't that hard to understand. ```putStrLn``` is just a monadic function which happens to be in the ```IO``` monad, meaning that it takes in a particular string (the string to be printed), outputs a unit value (of no significance), and does "something else" (in this case, it interacts with the terminal so as to print out the string, because that's what the ```IO``` monad allows you to do).

The type of ```getLine``` is harder to understand. ```getLine``` is a monadic value. But if we think of it as a monadic function of type ```() -> IO String``` then it makes sense: it is like a function that takes a unit input value (of no significance) and returns a ```string``` value, in the process interacting with the terminal (which is how it can determine which string to return; it returns whatever you type on a line).

And yet, Haskell doesn't give getLine the type ```() -> IO``` String; it gives it the type ```IO String```. So a monadic value is essentially a monadic function with an implicit input argument of type ```()```. As I said above, a lot of Haskell cognoscenti refer to this as an "action", so when they say that getLine is an "action" which does some terminal input/output and returns a string, that's what they mean. And that's about as much meaning as you can give to monadic values. When I talk about state monads in a later article you'll get a deeper perspective on how something that looks like a value can act like a function.

In the [next article](monad_2.html) in this series, I'll discuss the two fundamental monadic operations, where they come from, and what they mean.













# ASIDE: CURRYING

Now, in practice, one-argument functions are not enough to do many things we might want to do. How do we specify two-argument functions? For instance, how would we define a function ```q``` that takes two integer arguments and returns the sum of the squares of the arguments? The body of the function is easy to write:

``` haskell
  q x y = x * x + y * y
```
but the type signature is odd. You might expect that it would look like this:

``` haskell
  q :: Int Int -> Int
```
or perhaps:

``` haskell
  q :: (Int, Int) -> Int
```
but in fact it looks like this:
``` haskell
  q :: Int -> Int -> Int
```
The ```->``` associates to the right, so this really means:
``` haskell
  q :: Int -> (Int -> Int)
```


Now this is getting curious. A function of two arguments like ```q``` is represented in Haskell as a function of one argument (```x``` in this case) which returns a one-argument function which takes the second argument (```y``` in this case) and returns the result value. This is OK because in Haskell, as in any functional language, it's legal to return functions as the return value from other functions (another way to put this is that in functional languages, functions are just another kind of data). This way of representing functions which take multiple input arguments as functions of single arguments which return functions is called "currying" (named after the logician Haskell Curry, whose first name is where the name "Haskell" comes from; it was also independently discovered by a man named Schönfinkel, so you can call it Schönfinkeling if you like). So, for instance, a function ```r``` of four arguments ```w, x, y,``` and ```z``` (all integers) which also returns an integer, would look like this:
``` haskell
r :: Int -> Int -> Int -> Int -> Int
r w x y z = ...  -- some function of w, x, y, and z
```
and because ```->``` associates to the right, this really means:
``` haskell
r :: Int -> (Int -> (Int -> (Int -> Int)))
r w x y z = ...  -- some function of w, x, y, and z
```

So here, ```r``` is a function of a single argument (an ```Int```, called ```w``` in the example) which returns a function of type ```(Int -> (Int -> (Int -> Int)))```. That function, when applied to an ```Int``` (```x``` in the example) returns a function of type ```(Int -> (Int -> Int))```. That function, when applied to an ```Int``` (```y``` in the example) returns a function of type ```(Int -> Int)```, and that function, when applied to another ```Int``` (```z``` in the example) returns an ```Int``` — the result of the entire function call ```(r w x y z)```, which is really ```((((r w) x) y) z)```. That's called currying, and Haskell functions automatically curry their arguments. Currying turns out to be quite handy because you can apply the arguments one at a time instead of all at once, and these partially-applied functions are often quite useful by themselves. It's also conceptually useful in that from now on, we only have to worry about single-argument functions in our discussion. Nice!


[^juxtaposition] <a target="_blank" href="https://translate.google.com/#en/hy/juxtaposition">կողքկողքի դնելը</a>
