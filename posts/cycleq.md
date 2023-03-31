---
title: Cyclic Proofs for Functional Programmers
date: April 2023
---

## Cyclic Proofs in Brief

Cyclic proofs are an alternative to traditional proof trees where, instead, the graph of proof nodes may contain cycles.
Recently, many cyclic proof systems have been developped for logics with some notion of a fixed-point such as those with inductive defined predicates or domains, including my [CycleQ](https://arxiv.org/abs/2111.12553) system designed for cyclic equational reasoning over functional programs.

Of course, not all such proof graphs will correspond to valid arguments as a node may be trivial justified by itself.
Thus cyclic proof theory distinguishes between *pre-proofs* that are locally well-formed but have no other requirements and proper *proofs* that have an additional global correctness property.
The well-formedness of a pre-proof means that the premises of a given node justify its conclusion.
If we could perform well-founded induction of the proof tree, this property would be sufficient as an invalid conclusion requires an invalid premise and so on until we reach a leaf and thus have a contradiction.
For cyclic proofs, however, we perform well-founded induction not over the proof tree itself but over the instances required of proof nodes.
The global correctness property ensure that the required instances are well-founded so that we can appeal to a similar argument and justify the validity of each proof node by descent infinie.
Intuitively, for any given instance of a proof node in a proper cyclic proof, we can extract a corresponding finite proof tree.

## Curry-Howard Correspondence

This may seem like a rather obscure area of proof theory but actually has a very close correspondence to everyday components of function programs.
To understand this connection we must first revist the curry-howard correspondence which interprets propositions as types and their proofs as programs of that type.
For example, to prove $p \wedge q$ it is necessary to provide a proof of both `p` and `q` hence the tuple constructor captures conjunction:

```haskell
and :: p -> q -> (p, q)
and p q = (p, q)
```

Similarly, the impliciation is interpreted as the function arrow (i.e. if you give me a proof of the hypothesis, I will give you a proof of the conclusion), disjunction as the coproduct type, universal quantification as polymorphism or dependent types, and so on...

Where this gets interesting, however, is in the interpretation of proof-by-induction.
The induction principle for natural numbers is captured the following implication:

$$
  (P(0) \wedge \forall k.\, P(k) \Rightarrow P(k+1)) \Rightarrow \forall n.\, P(n)
$$

Translating this formula across the Curry-Howard correspondence, gives us the type `(n :: Nat) -> p 0 -> (forall k. p k -> p (k + 1)) -> p n` where `p :: Nat -> Type` is the predicate in question.
Clearly, there is only one natural definition for a function of this type that makes the underlying idea of proof-by-induction explicit:

```haskell
induct :: (n :: Nat) -> p 0 -> (forall k. p k -> p (k + 1)) -> p n
induct 0 base hyp = base
induct (n + 1) base hyp =
  let pn :: pn
      pn = induct n base hyp 
   in hyp pn
```

To get an intuition for the behaviour of this function by dropping the dependent types and suppose `p` is simply a proposition, we get the following function:

```haskell
foldN :: Nat -> p -> (p -> p) -> p
foldN 0 base hyp = base
foldN (n + 1) base hyp =
  let pn :: p
      pn = foldN n base hyp
   in hyp pn
```

i.e. the eliminator for natural numbers.
The same correspondence applied to other inductive datatype.
For example, structural induction over lists is interpreted computationally as the standard `foldr` function.

Standard proof systems with induction rules (or axioms) posit the existence of such eliminators that can be used in other code.
Their soundness is derived from the fact that the proof cannot be recursive and can only use these pre-determined recursion combinators.
Cyclic pre-proofs, on the other hand, are functional programs with general recursion.
The global correctness criteria, asserting the required instances of proof nodes forms well-founded sequences, corresponds to the fact that the program is terminating.

## Implications for Global Correctness Checking

The original mechanism for determining the soundness of a cyclic pre-proof was to use Büchi automata to capture the possible paths throught the proof (i.e. runs of the program) and show that each of these have an infintely decreasing value.
This process is costly as it requires constructing the complement of a Büchi automata, which is doubly exponential in the size of the proof.

Using the correspondence with termination of function programs, however, opens up the possibility of using efficient termination tools to justify cyclic proofs such as [size-change based termination](https://doi.org/10.1145/360204.360210).
This is the approach we employ in the CycleQ theorem prover.