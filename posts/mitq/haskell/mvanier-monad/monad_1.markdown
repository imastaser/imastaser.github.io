---
title: Yet Another Monad Tutorial (part 1: basics)
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

Now, using the ```$``` operator for function application isn't technically necessary because you can just juxtapose the function with its argument to apply the function to the argument (though there are actually some common uses for ```$``` involving operator precedence that we won't get into here). Interestingly, we can also define a "reverse apply" operator (which we'll call ```>$>```) that is like ```$``` but takes its arguments in the reverse order:
``` haskell
  (>$>) :: a -> (a -> b) -> b
  x >$> f = f x  -- = f $ x as well
```

This is somewhat appealing in that we can read it as "take a value x, apply the function f to it, and get the result". If you know unix, you may notice that the unix shell's pipe (```|```) operator works this way — you produce some data and then apply a program to it to transform it in some way. We can use whichever function application operator is more convenient for our purposes at any given time, though usually we don't use an operator at all, just juxtaposition.

### Function composition

Now that we've talked about function application, the next important topic is function composition, and it's really important. Let's say we have two functions ```f``` and ```g``` and a value ```x``` with the following types:

``` haskell
  x :: a
  f :: a -> b
  g :: b -> c
```



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