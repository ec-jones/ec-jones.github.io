---
title: Regular Effects
date: June 2021
---

# Indexed Datatypes

In his [ICFP paper](https://strathprints.strath.ac.uk/51678/7/McBride_ICFP_2014_How_to_keep_your_neighbours_in_order.pdf) from 2014, Conor McBride gives an experience report on writing a correct-by-construction sorted data strucutre in the depdently-typed programming language Agda.
His first attempt involves so called *measures* which are also an important aspect of the [Liquid Haskell](https://ucsd-progsys.github.io/liquidhaskell/) system.
The following data declaration represents the list datatype enriched with a length measure. Informally, a refinement type `{ x | p(x) }` is inhabitted by an element `e` just if `p(e)` holds.

```haskell
data List a where
  Nil :: { xs :: List a | len xs = 0}
  Cons :: a -> (xs' :: List a) -> { xs :: List a | len xs = len xs' + 1 }
```

Being able to specify the length of a list at the type-level gives us a stronger static guarantee. The head function, for example, can be refined to require lists that are *provably* non-empty.
Measures provide a quick-fire way of refining algebraic datatypes that feels very natural - the length function is one of the first encountered by functional programmers.
Measures can be translated into a more algebraic specification with an *index* (an additional type parameter, like `n :: Nat`{.haskell}) that specifies the value of a measure, e.g.:

```haskell
type List :: Nat -> Type -> Type
data List n a where
  Nil :: List 0 a
  Cons :: a -> List n a -> List (n + 1) a
```

The practicality of these advanced type systems depends on the capacity for type checking and inference. 
Otherwise, there is no advantage over manual proofs of correctness.
Type checking ultimately boils down to equality on types: we need to be sure that the types of two terms *align*, and thus it is safe for data to flow from one point in our program to another.
<!-- In fact, this is such a complex issue that the whole field of [Homotopy Type Theory](https://homotopytypetheory.org) has sprung up to ask questions about what equality between types really means. -->
In a dependently typed language this involves equality between arbitrary program terms.
For example, the types `List (x + 1) a`{.haskell} and `List (1 + x) a`{.haskell} would ideally be considered equivalent, but in general, this is an undecidable problem.

While a direct translation into indexed types works well for simple measures.
McBride noticed that some styles of indexing are better at placating type checkers.
And, for complex measures, this becomes very important.
In the following quote "green slime" refers to measure expressions such as `x + 1`.

> We got the wrong answer because we asked the wrong question: What should the type of a subtree tell us? somewhat presupposes that information bubbles outward from subtrees to the nodes which contain them.
> In Milner's tradition, we are used to synthesizing the type of a thing.
> Moreover, the very syntax of data declarations treats the index delivered from each constructor as an output.
> It seems natural to treat datatype indices as measures of the data.
> That is all very well for the length of a vector, but when the measurement is intricate [...] programming becomes vexed by the need for theorems about the measuring functions.
> The presence of 'green slime' - defined functions in the return types of constructors is a danger sign.
>
>
> We can take an alternative view of types, not as synthesized measurements of data, bubbled outward, but as checked requirements of data, pushed inward.
> To enforce the invariant, let us rather ask "What should we tell the type of a subtree?".

In summary, the target type of a given expression should dictate what type its sub-expressions should receive.
This follows simply from the fact that type annotations appear at the top-level in our program and rarely on the leaves of syntax trees.
In this post, we will construct an indexed type for dictating algebraic effect protocols with exactly this property.

## Algebraic Effects

Algebraic effects are an approach to managing side-effects in a pure language of rising popularity. 
They provide several advantages over the traditional monad transformer formulism, but most noticeable is their indifference to the order (and scope) of effects; the decision only needs to made by the handler, not the effectful procedure.
This flexibility allows the design of more composable systems.

At the heart of algebraic effect systems is the free monad, defined as a generalisation of trees with leaves of the return type and branches shaped by some signature.
While the signature is often assumed to be a functor, for which this tree interpretation is most lucid, we will instead focus on the following definition:

```haskell
data Free f a where
  Pure :: a -> Free f a
  Bind :: f a -> (a -> Free f b) -> Free f b
```
This datatype allows us to lift primitive operations given by some signature `f :: Type -> Type`{.haskell} into arbitrary monadic expressions.
By using the following `State s a`{.haskell} signature, for example, we can build functions that manipulate a state of type `s`.
The second parameter of this datatype is the return type of its primitive operations.

```haskell
data State s a where
  Get :: State s s
  Put :: s -> State s ()
```

There is also a growing interest in indexing monads by a *grading* that provides some abstraction of the operations they perform.
This is no different from considering the length of the list as an abstraction over its value, and has the same benefits.
However, we further require that a monad's grading is a monoid, so it has a sensible notion of composition and identity that mirrors the monad's structure.

As we are concerned with the free monad a good place to start is with the free monoid, i.e. lists. 
Considering signatures that are themselves indexed by some type `i`, we can index monadic expressions as either the empty list for the `Pure`{.haskell} constructor, indicating that it has no effect, or by prepending the index of a primitive operation to those of the continuation for the `Bind`{.haskell} constructor.

```haskell
type Free :: (i -> Type) -> [i] -> Type -> Type
data Free f ix a where
  Pure :: a -> f [] a
  Bind :: f i a -> (a -> f ix b) -> f (i : ix) b
```

We can now enrich our effect signature with an index that distinguishes between different sorts of primitive operation.
In this case, to indicate whether it is a `Get`{.haskell} or `Put`{.haskell}.
And thus specify that shape of an effectful operation at compile time.

```haskell
data OpSort
  = G | P

type State :: Type -> OpSort -> Type -> Type
data State s sort a where
  Get :: State s G s
  Put :: s -> State s P ()

modify :: (s -> s) -> Free (State s) [G, P] ()
modify f = 
  Bind Get $ \s -> Put (f s)
```

## Regular Effect Protocols

This technique for statically prescribing a sequence of effects can be thought of as a protocol that defines the acceptable behaviour of an effectful program.
It is not, however, particularly useful because we have to know the *exact* sequence.
What would be a lot nicer is if we were able to describe a set of valid behaviours.
A convenient way of specifying a set of lists (i.e. a language) is as a regular expression.
For example, if we require that all `Get`{.haskell} operations are followed by a `Put`{.haskell} operation, we could write `Star (Unit G :. Unit P)`{.haskell} using the following DSL:

```haskell
data Regex i
  = Empty -- Empty language
  | Epsilon -- Empty string
  | Unit i -- Single character
  | Regex i :. Regex i -- Sequential composition
  | Regex i :|| Regex i -- Union of languages
  | Regex i :&& Regex i -- Intersection of languges
  | Star (Regex i) -- Kleene Star, i.e. unbounded iteration
```

A very satisfying property of regular expressions is that the language which remains after consuming a single character is another regular expression.
Let's unpack this briefly with an example.
If I start with the language `Star (Unit G :. Unit P)`{.haskell} and observe a `G`{.haskell}, then the following sequence of characters must satisfy `Unit P :. Star (Unit G :. Unit P)`{.haskell} What happens if I observe a  `P`{.haskell} instead?
Well the remaining language is `Empty`{.haskell} as no string following a `P`{.haskell} will satisfy our protocol (not even the empty string `Epsilon`{.haskell}).
This construction is called the *Brzozowski derivative* and will be key to ensuring the type of the overall tree, that is our protocol, can effectively dictate the type of the subtrees, which will be the derivative with respect to the operations encountered so far.

The Brzozowski derivative on type-level indices is defined by three type families (i.e. type-level functions).
The first `Nu`{.haskell} (read nullable) checks whether the language of a regular expression contains the empty string.
It returns `Epsilon`{.haskell}, if this is the case, and `Empty`{.haskell} otherwise.
As we wish to minimise the amount of equational reasoning the compile has to do, and in fact, we won't specify any of the equational laws of regular expressions, this function also explicitly simplifies the result via the `Force`{.haskell} type family, oppose to the usual presentation where the output is a regular expression that is merely equivalent to `Epislon`{.haskell} or `Empty`{.haskell}.

```haskell
type Nu :: Regex i -> Regex i
type family Nu r where
  Nu Empty = Empty
  Nu Epsilon = Epsilon
  Nu (Unit i) = Epsilon
  Nu (r :. s) = Force (Nu r :&& Nu s)
  Nu (r :&& s) = Force (Nu r :&& Nu s)
  Nu (r :|| s) = Force (Nu r :|| Nu s)
  Nu (Star r) = Epsilon

type Force :: Regex i -> Regex i
type family Force r where
  Force Empty = Empty
  Force Epsilon = Epsilon
  Force (Empty :&& s) = Empty
  Force (Epsilon :&& s) = Force s
  Force (Empty :|| s) = Force s
  Force (Epsilon :&& s) = Epsilon
```

The third type family, which defines the derivative operation, is `Delta`{.haskell}.
It takes two parameters: an observed operation `i`, and the regular expression `r`.
The output is a regular expression whose language contains strings that can be prepended with `i` to make an element of the language defined by `r`.

```haskell
type Delta :: i -> Regex i -> Regex i
type family Delta i r where
  Delta i Empty = Empty
  Delta i Epsilon = Empty
  Delta i (Unit i) = Epsilon
  Delta i (Unit j) = Empty -- Here we know that i and j are distinct
  Delta i (r :. s) =
    (Delta i r :. s) :|| -- Either i is consumed by r,
      (Nu r :. Delta i s) -- or r can be the empty string and i is consumed by s
  Delta i (r :&& s) =
    Delta i r :&& Delta i s
  Delta i (r :|| s) =
    Delta i r :|| Delta i s
  Delta i (Star r) =
    Delta i (r :. Star r) -- Unfold Kleene star
```

## Putting it all together

Now we have an appropriate way of passing type information to subtrees, we can define a free monad indexed not by a list of operations but by a regular language of operations.
In the case of `Pure`{.haskell}, we need to know that the empty string is accepted by the protocol.
Luckily, the `Nu`{.haskell} type family checks exactly that!
For the `Bind`{.haskell} constructor, we just need to take the derivative of the target protocol with respect to the observed operation to find a suitable protocol for the continuation.

```haskell
type Free :: (i -> Type) -> Regex i -> Type -> Type
data Free f r a where
  Pure :: Nu r ~ Epsilon => a -> Free f r a
  Bind :: f i a -> (a -> Free f (Delta i r) b) -> Free f r b

type Protocol :: Regex GetPut
type Protocol =
  Star (Unit G :. Unit P)

-- This function will type check.
safe :: Free (State Int) Protocol ()
safe = 
  modify (s + 1)

-- This function will not type check!
unsafe :: Free (State Int) Protocol ()
unsafe =
  Bind Get $ \s -> Pure ()
```

This indexed monad is quite neat because we can now statically verify that only one of these two functions actually meets our protocol.
However, it is limited to explicit sequences of operations because the derivative is computed with respect to a given primitive operations, not another protocol.
It is worth noting that regular languages are also closed under quotienting, a generalisation of the derivative operator that would allow for the composition of arbitrary effectful functions.
Nevertheless, this toy implementation does demonstrate that organising structures along algebraic lines can serve not just the programmer but the compiler too.