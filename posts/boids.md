---
title: Comonadic Boids
date: March 2023
---

Nature's ability to organise itself is not just impressive but often beautiful too.
When a group of starlings flock together, they create a mesmerising acrobatic performance. Apparently, we're not the only species that like to dance!

It's not just biologists that find the sight fascinating.
There is a whole field of computer science dedicated to abstracting and replicating these emergent behaviours with a variety of applications. 

Boids was one of the first algorithms designed to mimic flocking behaviour.
Each boid is governed by three simple forces:

*  Separation: Avoid other boids as to not cause a collision.
*  Cohesion: Stick together, i.e. move towards the average position.
*  Alignment: Follow the direction of the crowd.

While these forces affect individual boids, they are inherently dependent on the position of neighbouring boids — the context.
If we are to implement a simple boid simulation in a characteristically functional (read over-engineered) style, it is natural to apply the framework of _context-aware programming_.

## Context-aware Pogramming

Monads have been widely adopted in the functional programming community, but their dual, co-monads, are sadly not so popular.
Although monads play a broad and important role in category theory, for a programmer they are essentially a convention for adding structure to the output of a function.
In particular, they allows us to clearly delineate its "true" result `b` from any side-effects `m`:

``` {.haskell}
effectulFunction :: Monad m => a -> m b
```

Of course, this convention is meaningless without some laws. For monads, the laws concern the composition of effectful functions and the ability to lift pure functions into this richer context. Effectful functions must behave somewhat like pure functions in that composition is associative and has an identity. 

``` {.haskell}
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
```

Similarly, comonads add structure to the input focusing on some subsection of the input `a` distinguished from contextual information ``w``.
In our case, the focus is an individual boid, whereas the context contains the relative position of the rest of the flock, i.e. resources that influence but are not influenced by the updating of an individual boid.
Ultimately, the distinction will depend on the application as with monads. 

``` {.haskell}
coeffectfulFunction :: Comonad w => w a -> b
```

As you might expect, comonads must also satisfy a set of analogous laws.
In order to understand the dual notion of composition, we can think of a comonad as a sort of *container*.
Within this container, there are many *positions* each holding an objects of type `a` that have some relation to each other such as occupying neighbouring cells of a grid.
One of the positions is the current focus.
However, we could focus on any of them, and they will all have a different view of their context, e.g. a different set of neighbouring cells. 
Comonads must come equipped with a function for *extracting* the current focus while disregarding the context.
And one for *extending* a context-aware operation to the object in each position in the container with their localised view of the context.

```{.haskell}
class Comonad w where
  extract :: w a -> w
  extend :: (w a -> b) -> w a -> w b
```

Under this intuition, the laws governing comonad instances can be roughly translated as:

* Extracting the focus from every position in a container must equate to the original container
  ```{.haskell} 
    extend extract = id
  ```

* Extending a context-aware operation to the whole container and then extracting the focus must be the same as applying the context-aware operation to the original container with its original focus.
  In other words, extend does not change which element is in focus.
  
  ```{.haskell}
    extract . extend f = f
  ```

* And finally, the composition of extended context-aware operations should be associative.
  
  ```{.haskell}
    extend f . extend g = extend (f . extend g)
  ```

## The Flock Comonad

Our three boid update functions have a natural focus and context: the current boid we're updating, and the rest of the flock.
We can think of a flock as a container full of boids where the position of each boid is its coordinate, in say 2-dimensional space.
Interestingly though, to perform these operations, we do not need exact positions just the relative positions of other boids.
The boid in focus is thus positioned at the origin, i.e. the zero vector (0, 0), and the context contains vectors to the other boids.

```{.haskell}
type Flock a = Map (Float, Float) a

extract :: Flock a -> a
extract flock = flock ! (0, 0)

extend :: (Flock a -> b) -> Flock a -> Flock b
extend f flock =
  mapWithKey (\pos _ -> f $ mapKeys (- pos) flock) flock 
```

\

![Application of `extend id` to a small flock.](/resources/boids.svg)

It's in the extension of context-aware operation the real magic happens.
For simplicity, let's consider extending the identity function, this will replace each boid with its view of the flock.
However, as our boids are rather egotistic and see themselves as the centre of the flock, their view of the flock will be inversely shifted by their own position so that they occupy the origin.

In the diagram on the left, the focus of a flock is indicated by a red circle.
After the context-ware update, each position in the flock is occupied by a copy of the original flock except where the focus has shifted to the position in question.

## Comonadic Boids

What is the payoff from all this book-keeping? We can now define the forces that act upon boids naturally - one boid at a time, without worrying about how this affects the overall flock.
This separation of concerns highlights the fact that self-organisation is an emergent property.
Consider the implementation of the cohesion force for example:

```{.haskell}
-- N.B. the return type is not Flock Boid
cohesion :: Flock Boid -> Boid
cohesion flock =
  let boid :: Boid -- Boid in focus
      boid = extract flock
      
      pos :: (Float, Float) -- Average position
      pos = sum (Map.keys flock / size flock)
    in boid `steerTowards` pos

applyCohesion :: Flock Boid -> Flock Boid
applyCohestion = extend cohesion
```

The type of this function hints that it is a force applied to boids rather than an arbitrary operation on flocks.
It is not possible, for example, to insert a new boid into the flock with a function of this type.
Although type-driven development is somewhat of an aesthetic concern, I do believe that a sprinkling of abstraction can lead to more lucid code that is consequently easier to maintain.
Ultimately the right representation of data should take into consideration the relevant operations and obscure dangerous or unintended ones; this is often achievable in an elegant manner by appealing to mathematical structures.