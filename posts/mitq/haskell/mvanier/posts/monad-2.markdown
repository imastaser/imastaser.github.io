---
title: Yet Another Monad Tutorial (part 2: >>= and return)
tags: mvanier, monads, tutorials, haskell
date: 2010-07-25
author: Mike Vanier
description: Mike Vanier's article about moands - part 2
comments: true
toc: true
---
[original article](http://mvanier.livejournal.com/4305.html)

In the [previous article](monad-1.html) I gave the conceptual background necessary to understand what monads are. Now I'm going to get into more of the details.

## The two fundamental monadic operations

Remember when I said above that monads generalize function composition and function application? We'll work through that here. Have patience: it'll take a while.

By this point, I hope you have at least a vague sense of what monads "are" and what they are used for. However, as I said before, one of the keys to functional programming is the ability to compose functions to create new functions. Functional programmers talk about "composability" all the time, with the implication that if some aspect of a programming language isn't composable, it's probably not worth much. So if our newly-minted monadic functions were not composable, they wouldn't be nearly as useful as they would be if they were. But as we'll see, they aren't composable using the standard Haskell function composition operator. Something more will be needed, and this will lead us to derive the two fundamental monadic operations (or at least their types).

Let's say that we have two monadic functions:
``` haskell
f :: a -> m b
g :: b -> m c
```  
for some monad ```m```. If you want a more specific example, you can imagine that ```f``` and ```g``` are in the ```IO``` monad, so we'd have

``` haskell
f :: a -> IO b
g :: b -> IO c
```

but the same argument will apply for all monads. Remember that (for the ```IO``` case) the function ```f``` takes a value of type ```a``` and outputs a value of type ```b```, possibly doing some (file or terminal) I/O along the way. Similarly, ```g``` takes a value of type ```b``` and outputs a value of type ```c```, possibly doing some I/O along the way. Therefore, if we wanted to compose them, we'd hopefully end up with a function like this:

``` haskell
h :: a -> IO c
```

*i.e.* a function that takes a value of type ```a```, outputs a value of type ```c```, and possibly does some I/O along the way (with the I/O somehow being the combination of the I/O activity for functions ```f``` and ```g```). We can write this out as follows:

``` haskell
compose: 
  (f :: a -> IO b) 
with: 
  (g :: b -> IO c) 
to get: 
  (h :: a -> IO c)
```   

However, our normal Haskell function composition operators won't work for this purpose, because they don't want the ```IO``` in the types. Let's compare with similarly-typed pure functions ```p```, ```q```, and ```r``` that don't do I/O:

``` haskell
p :: a -> b
q :: b -> c
r :: a -> c 
```  

Then we could compose them using either the ```(.)``` or the ```(>.>)``` operator as described above:

``` haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
r = q . p
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
r = p >.> q
```

Neither ```(.)``` or ```(>.>)``` will work with our monadic functions:

``` haskell
f :: a -> IO b
g :: b -> IO c
h :: a -> IO c
g . f     --> type error! mismatch between IO b and b
f >.> g   --> type error! mismatch between IO b and b
```  

### `mcompose`
The point is, you can't use a monadic value of type ```IO b``` when a type of ```b``` is needed. (This is a very common bug when writing monadic Haskell programs.) What we want is a special monadic composition function which I'll call `mcompose` (standing for "monadic compose") which has the following type signature:

``` haskell
mcompose :: (a -> m b) -> (b -> m c) -> (a -> m c)
```
This will work for any monad ```m```, including the ```IO``` monad. Specialized to the ```IO``` monad, it will have the following type signature:

``` haskell
mcompose :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
```  
Then we could use it like this:


``` haskell
f :: a -> IO b
g :: b -> IO c
h :: a -> IO c
h = f `mcompose` g
```  
and `h` would have the correct type signature. (We're using a spiffy syntactic feature of Haskell here, whereby any two-argument function can be turned into an infix operator by putting backquotes around it. Remember, operators in Haskell are just functions which happen to be placed between their operands.) Somehow, through (currently) mysterious means, the `mcompose` function (or operator, if you like) is able to

1. take the original input value of type `a`
2. apply ```f``` to it (this is just normal function application) to get a result of type ```IO b```
3. take the value of type ```IO b``` output from ```f``` and extract the value of type ```b``` (this is what we couldn't do before)
4. take the value of type ```b``` and apply ```g``` to it (again, this is just normal function application) to get the value of type ```IO c```, which is the result.


### `extract`

The only thing we can't already do is step (3), extracting a value of type `b` from a value of type `IO b`. Now, we could do this if we had a function called `extract` with this type:

``` haskell
extract :: IO b -> b
```
or more generally for arbitrary monads,

``` haskell
extract :: m b -> b
```  
It turns out that such a function, if it existed, would destroy all the advantages of monads and pure functional programming! One of the reasons we wanted monads in the first place was to keep these special notions of computation (monadic functions) separate from normal (pure) functions, because otherwise there would be no way to guarantee that pure functions were in fact pure. This is an important point, so I'm going to spend a little bit of time on it, after which we'll return to monadic composition.

> **Side note:** In fact, some monads do have the equivalent of an ```extract``` function, and for most of those monads it doesn't cause problems. All I'm saying is that a generic ```extract``` function that works for all monads is not allowed.

What we would like is to ensure that functions that have non-monadic type signatures are pure functions. Now, in a sense, even our monadic functions are pure functions, because they are implemented in Haskell as pure functions that return monadic values. But what we want to guarantee is that non-monadic (pure) functions don't even do that *i.e.* don't even return monadic values. If that's the case, they are certainly going to be pure functions. So a pure function ```hh``` of type

``` haskell
hh :: a -> c
```
should never do (file or terminal) input/output, for instance, because if it did it would be required by the type system to have the type

``` haskell
hh :: a -> IO c
```  

instead. Guarantees like this, enforced by the type system, are one of the major strengths of Haskell. They allow us to glance at the type of a function and be 100% sure that that function doesn't do input/output, for instance.


However, if we had the `extract` function, we could comp

``` haskell
ff :: a -> IO b
gg :: b -> c
hh = ff >.> extract >.> gg  -- or equivalently: hh = gg . extract . ff
```
So even though `hh` is never supposed to be doing I/O, if there was an `extract` function then you could build an ```hh``` function using normal function composition, it would have the type signature of a pure function, and yet it would do I/O. So much for separating I/O (and other monadic computations) from pure computations (recall that this was one of the main reasons for wanting monads in the first place). Note, by the way, that this is exactly the situation in most conventional programming languages, which is why the type systems of those languages can offer no guarantees that a function is pure. In Haskell we like pure functions and we use the type system to give us guarantees that pure functions are actually pure — and that means no ```extract```function.

There's one slight problem with what I just said: technically, it's a lie. There is a function called unsafePerformIO that has the type   IO a -> a   i.e. it's an extract function for the IO monad only. The word "unsafe" is a clue that tells you that you should avoid using it unless you know exactly what you're doing and are prepared for weird failures. I myself have never needed to use unsafePerformIO, but there are legitimate uses for it (for instance, deep down in the implementation of Haskell compilers). Just forget I even brought this up, OK? It's embarrassing. Excuse me while I go wash my hands.

OK, I'm back. So far, we've established that (a) we want to be able to compose monadic functions, (b) we can't do that with normal function composition in Haskell because we can't convert monadic types into regular types, and (c) we can't define an extract function to do that conversion, because that would screw up the purity of the rest of the language. So what do we do?

### `mapply`
Well, first of all, note that we can get by with something simpler than an `mcompose` function. Let's say we had an `mapply` (monadic apply) function that had this type signature:
``` haskell
mapply :: m b -> (b -> m c) -> m c
```
or, more specifically for the IO monad:
``` haskell
mapply :: IO b -> (b -> IO c) -> IO c
```  

It's called ```mapply``` because it's very similar to the regular function application operators. For instance, recall the ```>$>``` operator we defined previously, which had this type signature (using ```b``` and ```c``` instead of ```a``` and ```b``` for type variables):

``` haskell
(>$>) :: b -> (b -> c) -> c
````

This is the same as ```mapply``` except that the ```m```s are gone (the types are not monadic types). With ```mapply```, we could trivially define ```mcompose``` as follows:
``` haskell
mcompose :: (a -> m b) -> (b -> m c) -> (a -> m c)
mcompose f g x = (f x) `mapply` g  -- or: mapply (f x) g
```

Note that since the `->` associates to the right in type signatures, the type signature of mcompose can be written without the final set of parentheses as:
``` haskell
mcompose :: (a -> m b) -> (b -> m c) -> a -> m c
mcompose f g x = (f x) `mapply` g
```

This may be easier to understand than the previous version, but they are equivalent. Note that `x` has type `a` and the result has type `m c`. So what we're doing here is applying `f` to `x` to get a value of type `m b`, and using `mapply` on the `m b` value and the `g` function to get a value of type `m c`. So the upshot is, we don't need `mcompose` to be defined for us if we have `mapply`, because we can use `mapply` to define `mcompose` ourselves. And, in fact, `mapply` is one of the two fundamental monadic operations. It's normally called "bind" and is written as an infix operator with the symbol `>>=` as follows:
``` haskell
(>>=) :: m a -> (a -> m b) -> m b 
```

### `>>=`
Note that I did a switch in the type signature, using `a` in place of `b` and `b` in place of `c`. It doesn't matter since `a`, `b`, and `c` are type variables — they work for any types.

I'd just like to point out here that `>>=` has an incredibly abstract type. Its first argument is a value of type `m a`, where `a` can be any type at all and `m` is any monadic type constructor whatsoever. The second argument is a function of type `a -> m b`, where `a` and `b` can be any types at all and `m` is again any monadic type constructor. The return value has type `m b`, where again `b` can be any type and `m` is any monadic type constructor. When you program in Haskell for long enough, this kind of type signature becomes second nature, but it can be intimidating to new Haskell programmers. If you specialize it to the `IO` monad, you get:
``` haskell
(>>=) :: IO a -> (a -> IO b) -> IO b
```
which, of course, is the type signature of an `IO`-specific monadic apply operator. We'll see below that Haskell's type class mechanism allows us to use the same operator name `>>=` for all the different specializations of this operator to different monads (how cool is that?).

Assuming we have the `>>=` operator, we can now compose `f` and `g` to get `h` as follows:

``` haskell
 -- assume we have:
  f :: a -> m b
  g :: b -> m c

  -- definition of h:
  h :: a -> m c
  h x = f x >>= g
```  
We can also write `h` directly as:
``` haskell
h = \x -> f x >>= g
```

where the `\x -> ...` is, as I mentioned above, Haskell's notation for an anonymous function (in this case with a single argument `x`); both versions of `h` mean the same thing. Using `mcompose` we can write this as:
``` haskell
h = f `mcompose` g = mcompose f g = \x -> (f x >>= g)
```

### `>=>`
Our definition of `mcompose` is thus just:
``` haskell
mcompose f g = \x -> (f x >>= g)
```
and in fact, Haskell has a standard operator for monadic composition called >=>:
``` haskell
 f >=> g = \x -> (f x >>= g)  -- same as (f `mcompose` g) but more concise
```
So, assuming we have this monadic apply operator `>>=`, we can easily define the monadic composition operator `>=>`. So the monadic apply operator (the bind operator) is the important concept here. As we'll see, each individual monad has to define its own specific version of this operator, which will be different from every other monad's version. That's where Haskell's type classes will come in very handy. Incidentally, in the <a href="https://www.haskell.org/ghc/" target="_blank">ghc</a> Haskell compiler, the `>=>` operator is defined in the `Control.Monad` module.

### `=<<`
Now remember that we could write the normal apply operator in two ways:
``` haskell
  ($) :: (a -> b) -> a -> b
```  
and
``` haskell
  (>$>) :: a -> (a -> b) -> b
```  
depending on what order we wanted the arguments to be in. (Of course, it's also fine to define both operators and use whichever one is most convenient in any given situation.) Similarly, we can write the monadic apply operator in two ways. The first way is as the bind operator `>>=` with type
``` haskell
  (>>=) :: m a -> (a -> m b) -> m b
```  
which is analogous to the non-monadic `>$>` apply operator. We can also trivially define a monadic apply operator that takes its operands in the reverse order:
``` haskell
  (=<<) :: (a -> m b) -> m a -> m b
  f =<< x  =  x >>= f
```
You can also use the `flip` function, which takes a function of two arguments and returns a new function which is the same as the old one except that it takes the arguments in the reverse order:
``` haskell
  flip :: (a -> b -> c) -> (b -> a -> c)
  flip f = \x y -> f y x
```
Then we can define `=<<` as follows:  
``` haskell
 (=<<) = flip (>>=)
```
You get extra points for functional coolness if you write concise definitions like this.

Similarly again, we can define a monadic composition operator that takes its operands in the reverse order:

``` haskell
(>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)  -- already defined

(<=<) :: (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) = flip (>=>)
```

So just as was the case for the regular (non-monadic) `apply` and `compose` operators, we can define monadic apply and compose operators which take their operands in whichever order we want. In practice, though, the monadic operator Haskell programmers seem to use the most is the `>>=` operator (or at least, it's the one I use the most).

If you've understood everything so far, congratulations! It's all downhill from here. Or so I hope.

### `functionToMonadicFunction`
There is one more fundamental monadic operation I need to talk about. To motivate it, consider this scenario. You want to compose a monadic function with a non-monadic function. In other words, you have these functions
``` haskell
f :: a -> m b   -- monadic
g :: b -> c     -- non-monadic
```
The problem is this: you can't use regular function composition to compose `f` and `g`, because `m b` is not the same type as `b`. And you can't use monadic function composition either, because `g` doesn't have the type `b -> m c`, which is what monadic composition would require. So what can you do?

If we had the `extract` function I described above, you could compose the two functions the way I showed there:

``` haskell
h :: a -> c
h = f >.> extract >.> g
```
but as I mentioned, you're not allowed to do this. In other words, you're not allowed to compose a monadic function with a non-monadic function to get a non-monadic function (because that would screw up the functional purity of the language). What you are allowed to do is to compose a monadic function with a non-monadic function to get a monadic function, which would work like this:
``` haskell
h :: a -> m c
h = f [somehow composed with] g
```

Now, we know we can't use our monadic composition operator for this, because `g` doesn't have the right type (which would be `b -> m c`). But we could use the monadic composition operator if we had some way to convert a non-monadic function to a monadic one. In other words, if we had a function `functionToMonadicFunction` with this type signature:
``` haskell
functionToMonadicFunction :: (b -> c) -> (b -> m c)
```
then we could define `h` as:
``` haskell
h :: a -> m c
h = f >=> (functionToMonadicFunction g)
```
It turns out that all you need in order to define `functionToMonadicFunction` is an even simpler thing, which is a monadic function with the (possibly confusing) name of `return`. It has the following type signature:
``` haskell
return :: a -> m a
```
for any type `a` and any monadic type constructor `m`. What `return` does is convert a regular value into the corresponding monadic value for a given monad `m`. We'll see the specifics of this below.

If you have `return`, then `functionToMonadicFunction` can trivially be defined as:
``` haskell
  functionToMonadicFunction :: (a -> b) -> (a -> m b)
  functionToMonadicFunction f = \x -> return (f x)    
```
or, if I wanted to be cool and use function composition, as:
``` haskell
  functionToMonadicFunction :: (a -> b) -> (a -> m b)
  functionToMonadicFunction f = return . f
```
or even as:
``` haskell
  functionToMonadicFunction :: (a -> b) -> (a -> m b)
  functionToMonadicFunction = (return .)  
````
using a cool syntactic feature of Haskell called *operator sections*. All three functions are equivalent.  

Note that I once again switched `b` for `a` and `c` for `b` in the type signature of `functionToMonadicFunction`; again, it doesn't matter. The point is, with this `return` function, we can now compose monadic functions with non-monadic ones to create new monadic functions. And `return` is the second fundamental monadic operation.

### `return`
> **Side note:** If you've done a lot of imperative programming, you will probably find the name `return` more than a little annoying at first. Just remember that it is not a keyword in Haskell and it has nothing to do with returning from a function. So try to keep those ideas out of your head when dealing with return.

Let's put `return` to work composing our monadic function `f` with the non-monadic function `g` to get the monadic function `h`. Here's the definition:
``` haskell
  h = f >=> (return . g)
```  
because, as we saw above, `return . g` will convert `g` into a monadic function.

After all this, you might wonder how many more monadic operations we're going to have to plow through before we're done defining them all. As Professor Farnsworth would say: Good news, everybody! There are only two! There are also a couple of non-critical operations that we will eventually want to define for convenience, but `>>=` and `return` are the only ones that absolutely have to be there.

There is one rather peculiar aspect of `return`. We say that `return` has the type `a -> m a`, but when we say *e.g.* `return 10`, what is the type of the output? It could have the type `IO Int`, or `Maybe Int` or some other monadic type involving `Int`. How do we know which of the many possibilities is the correct one? Note, by the way, that the monadic value of type `IO Int` is a completely different value than the monadic value of type `Maybe Int`, so it's not just about getting the right type — it's not even obvious what kind of value return `10` *is!*

In Haskell, this is worked out by the context in which `return 10` is found. The type checker has to make sure that all the functions get the right type of input arguments, and so if `return 10` is the input to a function expecting a value of type `IO Int`, the type checker will decide that `return 10` has the type `IO Int` (and similarly for other monads). Put differently, the value computed by `return 10` depends on the type it has to have according to its context. If you want to, you can annotate `return 10` with the type you want it to have by writing `(return 10 :: IO Int)`, for instance, but this is rarely necessary.

### To recap this section:

* There are two fundamental monadic operations, called `"bind"` (the `>>=` operator) and `return`.

* The bind (`>>=`) operator is a monadic apply operator. It can be used to define a monadic composition operator, which is written `>=>`.

* The `return` operator transforms regular values into monadic values. It can be used to define a function to convert regular functions into monadic functions.

## What do monadic application and composition *mean*?









