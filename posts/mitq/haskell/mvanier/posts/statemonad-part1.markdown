---
title: Haskell state monads, part 1
tags: mvanier, monads, tutorials, haskell
date: 2008-01-21
author: Mike Vanier
description: Mike Vanier's article 
comments: true
toc: true
---
[original article](http://mvanier.livejournal.com/1765.html)

I found the concept of state monads in Haskell quite hard to grasp for a long time, so I thought I'd give a (hopefully) brief description of what they are, why they're useful, and how they work. In addition to being useful and interesting by themselves, understanding state monads will make it much easier to understand the ```IO``` monad, which is how input/output is done in Haskell (and which is thus one of the most useful monads).

**First question:** Why do we need anything like state monads? Why not just handle state the way it's handled in most programming languages?

Most programming languages are essentially imperative, which means that they have a notion of a collection of mutable variables (local, global, or some other kind e.g. inside an object), and much of the computation consists of altering the values of these variables (which we in the functional programming community call "mutation", generally pronounced in a way so as to suggest that something obscene and unholy is going on). In essence, in imperative languages, variables vary. But functional languages, and more specifically pure functional languages like Haskell, either don't allow this at all or at least strongly discourage it. Why is that? It's because functional languages are built on a notion of functions that is identical to the mathematical notion of functions, where you give a certain set of inputs and the function will always produce the same outputs for those inputs. This property is called "referential transparency". Having mutable variables, and most especially mutable global variables, makes this property much harder to enforce. Referential transparency is important because it makes functions easy to reason about; you don't have to consider the current state of a lot of global variables or object instance variables when considering if your function is correct or not, so debugging is much easier. This isn't to say you can't write a referentially-transparent function in an imperative language -- you can and people do all the time. It's just that you have no guarantee that any given function is referentially-transparent, which is a bad thing (kind of like driving without seat belts). And even if you could prove that your spiffy new function (which has lots of mutation of, say, local variables) is referentially transparent, it's still probably going to be harder to prove that it's correct than it would be to prove the correctness of a function without mutation. One of the main reasons for using a functional language is to write programs that are easy to prove correct. If you want that, mutable state is not your friend.

It turns out that in imperative languages, a huge part of the code is involved with changing the value of some variable. I would estimate that at least 50% of the code in a typical C program changes the value of some variable. For instance, every assignment statement changes a variable's value. And now I tell you that you have to live without this? Am I on crack or something? Well, I hope not (is my parole officer reading this?), but it's clear that at the very least, writing functions in a functional style is going to be different from writing them in imperative style. This is covered quite well in a number of books, of which my favorite is Abelson and Sussman's [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/sicp) (though that book uses the Scheme language, not Haskell). However, I can give you a flavor for the issues here. Consider a function which takes in two positive integers and computes the greatest common denominator of the two integers using Euclid's method. In C, such a function would look like this:


``` c
int gcd(int x, int y) 
{
    while (x != y) 
    {
        if (x < y)
        {
            y = y - x;
        }
        else
        {
            x = x - y;
        }
    }

    return x;
}
```

The details of the algorithm or why it works aren't important for the points I'm going to make, so I won't go into that here (read Euclid or Knuth or look it up on Wikipedia if you're curious). What is interesting is that this function contains two mutable variables (x and y), and computes the result by mutating those two variables. So mutation is critical to make this particular function work the way it does.

Now, how would you write a function like this in a functional language where mutation wasn't allowed? The standard way to do this is to use recursion, and the resulting function would look something like this:

``` haskell
gcd :: Int -> Int -> Int
gcd x y | x == y    = x
        | x < y     = gcd x (y - x)
        | otherwise = gcd (x - y) y
```

This Haskell function is using the same algorithm as the C version, but instead of having a while loop it uses recursion (the same trick will also work in C, but recursion is implemented more efficiently in most functional languages than it is in imperative languages so you don't want to do it that way in C).

OK, fine. I've just shown that for some functions at least, there are ways to get around the absence of mutation. (The Abelson and Sussman book I referred to above goes into this style of programming in great detail if you want to learn more about it -- and if you're a right-thinking person i.e. one who thinks like me, you do.) So this begs a different question: if this is the case, why even bother with stateful computations? 

Why not write everything using this neato recursion trick or some variation of it?
**There are basically two answers to this question:**

* Some computations may just be more naturally expressed in terms of state mutation than in other way. To write them in terms of recursion might be quite tedious and difficult.

* If you write a computation in terms of explicit state mutation, a clever compiler might be able to generate really fast code out of that, because computers are basically big imperative state mutation machines.


For these reasons, there is value in supporting some way of doing stateful computations that involve mutation, even in functional languages. Many functional programming languages like [Scheme](http://www.schemers.org/) or [Ocaml](http://www.ocaml.org/) therefore offer constructs to do imperative programming (mutation of state variables) as fundamental constructs of the language. By doing so, they can no longer be considered pure functional languages, and are thus usually called "impure". "Impure" doesn't mean "bad", but it does mean that the nice properties that characterize programs written in pure functional languages may not hold in these languages. Basically, these languages give you a back door or escape hatch and allow you to program in an imperative style if you want to. Haskell, as a pure functional language, can't do that. So how does Haskell support these kinds of computations? The answer to this question is somewhat involved, but it will eventually lead us to the topic of state monads.

First of all, you should realize that any function that involves some mutable state can be rewritten in a form in which the mutable state variables becomes extra arguments to the function (or several extra arguments). So instead of this:

``` c
// pseudo-code, sort of javascript-ish
function foo(x, y) {
   // code which operates on x, y and some
   // internal state variable or variables,
   // possibly mutating any or all of them
}
```
you could write this:

``` c
// note the extra "state" argument
function foo(state, x, y) {
    // code which operates on x, y, 
    // and state only
}
```

or even this:

``` c
// multiple (3) state variables:
function foo(state1, state2, state3, x, y)
{
    // code which operates on state1, 
    // state2, state3 and x and y
}
```

This approach is called **"threading the state"** You take all the local or global variables that are used inside your function and make them extra arguments to the function.

In Haskell, you can take a bunch of values of any types and make a composite type that contains all of them. So in the above example, if ``` state1 ``` was of type ``` Int, state2 ``` was of type ``` Float ```, and ``` state3 ``` was of type ``` String ```, we could define a new type like this:

``` haskell
newtype FooState = FS (Int, Float, String)
```

The "FS" is a *type constructor;* it's basically a function which generates a new ```FooState``` value out of an ```(Int, Float, String)``` tuple. There are other ways to define ```FooState``` in Haskell, but they aren't any better and the differences aren't important here. What this means is that in Haskell, you can thread the state by using a maximum of one extra argument to a function, as long as you're willing to define a new type for the state variables for that function. So in Haskell, ```foo``` could be written like this:
``` haskell
Foo :: FooState -> Int -> Int 
       -> (Int, FooState)
Foo (FS (a, b, c)) x y =
    -- code that uses x, y, plus the three 
    -- components of FooState, 
    -- called a, b, and c here
```

Here I'm assuming that x and y are Ints, and that the entire function returns an Int. Since FooState may be modified by the function, the function also has to return that, so the return value is a tuple of an Int and a FooState.


The significance of this is that any computation involving mutation of state can be broken down into a bunch of smaller computations involving mutation of state, until you get down to very primitive state mutations. That's how state monads work in Haskell. This is a good place to stop, so I'll pick up the discussion in [part 2](./statemonad_part2.html).