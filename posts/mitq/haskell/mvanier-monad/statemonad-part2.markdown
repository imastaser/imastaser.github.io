---
title: Haskell state monads, part 2
tags: mvanier, monads, tutorials, haskell
date: 2008-01-21
author: Mike Vanier
description: Mike Vanier's article 
comments: true
toc: false
---
[original article](http://mvanier.livejournal.com/1901.html)

In my [previous post](statemonad_part1.html), I described in general terms how state transformations are represented in functional languages like Haskell that don't have the concept of mutable state (assignment to variables). There are different ways to do this, but the way that most closely simulates the way things work in imperative languages is by using state monads. This involves taking an imperative computation and breaking it down into very small subcomputations, each of which is a very simple state transformation. In this post, I'm going to expand on this, showing what I mean by "simple state transformation". I'm going to try to explain not only how state monads work in Haskell, but why they work the way they do.

To model an imperative computation in Haskell, you do this:

1. Break the whole state-modifying computation into small state-modifying transformations, expressed as functions called state transformers.

2. Figure out a way to combine (compose) the individual state transformers into a single big state transformer which represents the entire imperative computation. I'll call the resulting state transformer the "composite" state transformer.

3. Run the computation by taking an initial state and giving it as an argument to the composite state transformer. The result of this function application is the final state.

4. Get the result from the final state.

It's important to realize that the individual state transformers you work with are just ordinary functions that happen to act on the state in some way. Furthermore, they are pure functions, which means that they have no effects other than the way that they transform their inputs into their outputs (this is the mathematical notion of what a function is). Essentially, we've taken an impure computation (one where at least some of the statements implicitly alter the shared state) and turned it into a series of pure computations (where the alterations to the shared state are made explicit by passing the state as one of the arguments to each function, also known as threading the state) which are in turn composed to make a single pure computation. Then we "run" the pure computation (function) on the initial state to get the final state, and that final state includes our desired answer.

# Kinds of state transformer functions
Let's look at what the individual state transformer functions are going to look like. It stands to reason that they will involve the shared state in some way. Let's assume that the shared state has been packaged up into a single data type called ```State``` (we saw how we could do that last post using a ```newtype``` declaration). Our state transformer functions may also be working with arguments and/or results which are of types different from the ```State``` type; these could represent components of the state or something else. We'll informally refer to one such data type as being of type ```Value```; of course, in a real situation there could be multiple value types. The question becomes: what kinds of state transformer functions are there? We'll classify them by their type signatures. They could include:

1. Functions that take a state as input, modify the state, and return the new state as output. These will have type ```State -> State```.

2. Functions that take a state as input, leave it alone but extract some value from it, and return the extracted value. These will have type ```State -> Value```.

3. Functions that take a state and a value as input, modify the state, and return the new state. These will have type ```(State, Value) -> State```.

4. Functions that take a state as input, modify the state, and return the new state as well as some other value (possibly extracted from the state). These will have type ```State -> (State, Value)```

5. Functions that take a state and a value as input, modify the state, and return the state as well as a value (possibly extracted from the state). The output value may be of a different type than the input value. These will have the type ```(State, Value) -> (State, Value')``` where ```Value``` and ```Value'``` are two distinct types (though in some cases they may be the same).

# Generic scheme for composing state transformer functions

| # |                                         |                                         |          will use                 |
|---|-----------------------------------------|-----------------------------------------|---------------------------|
| 1 | ```State -> State```                    | ```State -> (State, ())```              | one of cases 3, 4, or 5 |
| 2 | ```State -> Value```                    |                                         | case 4           |
| 3 | ```(State, Value) -> State```           | ```Value -> State -> State```           | case 4                             |
| 4 | ```State -> (State, Value)```           |                                         |                           |
| 5 | ```(State, Value) -> (State, Value')``` | ```Value -> State -> (State, Value')``` | case 4                            |


That's quite a few distinct possibilities. If we want to be able to compose these functions with each other, then we have 25 different cases to consider (each of the 5 different type signatures combined with each of the others). If we want to have a generic way of dealing with state transformations, we would rather not have to deal with 25 different cases, so let's see if we can reduce this to something more manageable.

* case 2<br>
    First off, notice that case 2 can be considered a degenerate version of case 4, where the output state is the same as the input state. So we can discard state 2 altogether. It turns out that case 2 is used in what is called the "Reader" monad in Haskell, which models computations that have shared state that is read from but not mutated. We want something more general than that, so we'll just use case 4 in place of case 2.


* case 1 <br>
    Similarly, we can get rid of case 1 by realizing that Haskell contains the "unit" type which is a value that represents nothing. Technically, a ```unit type``` is a type with only one value; in Haskell, that value is represented by an empty tuple, or ```()```. So we could replace case 1 by any of cases 3, 4, or 5 by making the Value type be the unit type. We'll choose to replace it with case 4, so that if we have a function of type ```State -> State```, we'll replace it by a function of type ```State -> (State, ())```, which is of the same form as case 4. Note that the ```Value``` type doesn't represent a specific type but simply stands in for any type we might want to use. Here, we use the unit type ```()``` in place of ```Value```.

* cases 3 and 5 <br>
    At this point, we will use a trick to get rid of cases 3 and 5, so that we'll be left with only case 4. Both cases 3 and 5 have the input argument ```(State, Value)```, which is a tuple composed of the state along with some value. We can break up this tuple so that case 3 has the type ```Value -> State -> State``` and case 5 has the type ```Value -> State -> (State, Value')```. This is perfectly usable in practice; it just means that instead of using functions that take two arguments in a tuple, we use functions that take one of the two arguments (the one with the ```Value``` type) and return a new function which takes an argument of type ```State``` and returns a tuple of type ```(State, Value')```. This process is called "currying" in functional programming (a reference to Haskell Curry, a pioneer in mathematical logic and the person the Haskell language is named after). What we're doing here is thus to take some kinds of state transformations and turn them into functions which take a value and return another function, which happens to be a simpler kind of state transformation. By doing this, we can make the state transformation functions all of the same type, which will be very useful when designing a generic scheme for composing them.

This previous paragraph may have been somewhat confusing, so feel free to reread it and email me any questions you might have. Make sure you understand it, because it's going to be critical in what follows.

# Summary of previous paragraphs

In summary, all of our state transformation functions are going to have the type signature ```State -> (State, Value)```, for some state and value types. We can define a new type for this as follows:

``` haskell
newtype StateTrans s a = ST (s -> (s, a))
```

This defines a new family of types called ```StateTrans``` (technically, this is called a *polymorphic* type which means that it's a type family parameterized on some other types). The ```s``` and ```a``` are *type variables*; substituting specific types into ```s``` and ```a```gives you a concrete (non-polymorphic) type. The ```s``` types represent the state types and the ```a``` types represent the value types. Typically, in a particular computation all the state types are the same (representing the shared state of the computation), while the value types can differ for each state transformer that takes part in the overall computation. One thing to note about this family of types is that a single "value" of type ```StateTrans s a``` is actually a function; in other words, ```StateTrans``` values are functions. This is fine in Haskell (and in any functional language), where functions are first-class values, but it does sometimes lead to some confusion because the same entity can be spoken of as a value or as a function.

One more little tweak: Haskell actually defines the above type like this:
``` haskell
newtype ST s a = ST (s -> (s, a))
```

In other words, the same name ```ST``` is used as the name of the type family and as the name of the type constructor. This is legal in Haskell; type names and type constructor names live in different namespaces. We'll use this definition from now on. For experts: yes, I know that there are strict and lazy versions of this polymorphic type, but I'll ignore that here.

If you look back a few paragraphs to the trick I used to get rid of cases 3 and 5, recall that those cases will be represented by functions of the type ```Value -> State -> (State, Value')``` (where ```Value'``` will be the unit type ```()``` for case 3). Expressing this in terms of our new ```ST``` types, we see that these cases are handled by functions of the type ```a -> ST s b```. All I've done here is substitute ```a``` for ```Value```, ```b``` for ```Value'```, ```s``` for ```State``` and ```ST s b``` for ```State -> (State, Value')``` (or, if you prefer, for ```s -> (s, b)```).

The story so far: all of our state transformers have one of two types:

1. ```ST s a```
2. ```a -> ST s b```

The first kind of state transformer is used for state transformers that may alter the state and return a value. The second kind is used for state transformers that do that, and in addition require an input value (which may be used to modify the state, for instance).

Our job now will be to figure out **a way to compose these state transformers** to make more complex state transformers. That will be where monads come in, and that will be the topic of later posts in this series.

Before I finish this post, I'd like to consider the question of why we can't have all of our state transformers be of type ```State -> State``` (case 1). If they were of that type, it would certainly be easy to compose them; you could just use regular function composition, and the output state of one state transformer would be the input state of the next state transformer. Furthermore, the trick I used to get rid of cases 3 and 5 will work here, so we can also handle state transformers of the type ```(State, Value) -> State``` by changing them to type ```Value -> State -> State``` which becomes a state transformer of type ```State -> State``` once you apply the first state transformer to a value of type ```Value```. However, if we do this our system has the severe limitation that you can't pull anything out of the state until the computation is done. This is bad because you often have to extract components of the state during a computation (for instance, in order to pass that component as an argument to a function and do something with the result). To get around this problem we either have to do one of two things:

1. Every time we want to pull a value out of the state, we stop the entire computation, extract the value, and restart the computation. This will make our code very tedious to write.

2. Alternatively, we can take any function which needs to work on a component of the state and turn it into a function of type ```State -> State``` (ignoring the parts of the state it doesn't need and putting the results back into the state appropriately), and then using that as a new state transformer that would be composed with the other state transformers. This would also be very tedious to write (and to understand).

Although I suppose this could be done, it turns out to be more straightforward to simply work with state transformers using the ```ST``` s a types described above.

That's all for this post. In the next post, I'm going to revisit the GCD function from the last post and show how its component parts can be expressed as state transformers, and then later I'll show how all of this gets put into the monad framework and how we would write that function using state monads.