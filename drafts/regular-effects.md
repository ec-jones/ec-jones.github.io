
  <main>
    <h2>
      Indexed Datatypes
    </h2>

    <p>
      In his <a href="https://strathprints.strath.ac.uk/51678/7/McBride_ICFP_2014_How_to_keep_your_neighbours_in_order.pdf">
      ICFP paper</a> from 2014, Conor McBride gives an experience report on writing a correct-by-construction sorted data
      strucutre in the depdently-typed programming language Agda. His first attempt involves so called <q>measures</q>,
      which are also an important aspect of the <a href="https://ucsd-progsys.github.io/liquidhaskell/">
      Liquid Haskell</a> system. The following data declaration represents the list datatype enriched with a length
      measure. Informally, a refinement type <span>{ x | &phi; }</span> is inhabitted by an element e if
      <span>&phi;[e/x]</span> holds.
    </p>

    <div class="code-line">
      <span class="purple">data</span> <span class="red">List</span> a <span class="purple">where</span>
      <br>
      &nbsp; <span class="blue">Nil</span> :: { xs :: <span class="red">List</span> a | len xs = 0 }
      <br>
      &nbsp; <span class="blue">Cons</span> :: a -> (xs :: <span class="red">List</span> a) -> { ys ::
      <span class="red">List</span> a | len ys = len xs + 1 }
    </div>

    <p>
      Being able to specify the length of a list at the type-level gives us a stronger static guarantee. The head
      function, for example, can be refined to require lists that are <em>provably</em> non-empty. Measures provide a
      quick-fire way of refining algebraic datatypes that feels very natural - the length function is one of the first
      encountered by functional programmers. Measures can be translated into a more algebraic specification with an
      <em>index</em> (an additional type parameter, like <span>n :: <span class="red">Nat</span></span>) that specifies
      the value of a measure, e.g.:
    </p>

    <div class="code-line">
      <span class="purple">type</span> <span class="red">List</span> :: <span class="red">Nat</span> ->
      <span class="red">Type</span> -> <span class="red">Type</span>
      <br>
      <span class="purple">data</span> <span class="red">List</span> n a <span class="purple">where</span>
      <br>
      &nbsp; <span class="blue">Nil</span> :: <span class="red">List</span> 0 a
      <br>
      &nbsp; <span class="blue">Cons</span> :: a -> <span class="red">List</span> n a -> <span class="red">List</span>
      (n + 1) a
    </div>

    <p>
      The practicality of these advanced type systems depends on the capacity for type checking and inference.
      Otherwise, there is no advantage over manual proofs of correctness. Type checking ultimately boils down to
      equality on types: we need to be sure that the types of two terms <em>align</em>, and thus it is safe for data to
      flow from one point in our program to another. In fact, this is such a complex issue that the whole field of
      <a href="https://homotopytypetheory.org">Homotopy Type Theory</a> has sprung up to ask questions about what
      equality between types really means. In a dependently typed language this involves equality between arbitrary
      program terms. For example, the types <span><span class="red">List</span> (x + 1) a</span> and
      <span><span class="red">List</span> (1 + x)</span> a would ideally be considered equivalent, but in general, this
      is an undecidable problem.
    </p>

    <p>
      While a direct translation into indexed types works well for simple measures. McBride noticed that some styles of
      indexing are better at placating type checkers. And, for complex measures, this becomes very important. In the
      following quote 'green slime' refers to measure expressions such as x + 1.
    <blockquote>
      We got the wrong answer because we asked the wrong question: <q>What should the type of a subtree tell us?</q>
      somewhat presupposes that information bubbles outward from subtrees to the nodes which contain them. In Milner's
      tradition, we are used to synthesizing the type of a thing. Moreover, the very syntax of data declarations treats
      the index delivered from each constructor as an output. It seems natural to treat datatype indices as measures of
      the data. That is all very well for the length of a vector, but when the measurement is intricate [...]
      programming becomes vexed by the need for theorems about the measuring functions. The presence of 'green slime' -
      defined functions in the return types of constructors is a danger sign.
      <br>
      <br>
      We can take an alternative view of types, not as synthesized measurements of data, bubbled outward, but as checked
      requirements of data, pushed inward. To enforce the invariant, let us rather ask
      <q>What should we tell the type of a subtree?</q>.
    </blockquote>
    In summary, the target type of a given expression should dictate what type its sub-expressions should receive. This
    follows simply from the fact that type annotations appear at the top-level in our program and rarely on the leaves
    of syntax trees. In this post, we will construct an indexed type for dictating algebraic effect protocols with
    exactly this property.
    </p>

    <h2>
      Algebraic Effects
    </h2>

    <p>
      Algebraic effects are an approach to managing side-effects in a pure language of rising popularity. They provide
      several advantages over the traditional monad transformer formulism, but most noticeable is their indifference to
      the order (and scope) of effects; the decision only needs to made by the handler, not the effectful procedure.
      This flexibility allows the design of more composable systems.
    </p>
    <p>
      At the heart of algebraic effect systems is the free monad, defined as a generalisation of trees with leaves of
      the return type and branches shaped by some signature. While the signature is often assumed to be a functor, for
      which this tree interpretation is most visceral, we will instead focus on the following definition:
    </p>

    <div class="code-line">
      <span class="purple">data</span> <span class="red">Free</span> f a <span class="purple">where</span>
      <br>
      &nbsp; <span class="blue">Pure</span> :: a -> <span class="red">Free</span> f a
      <br>
      &nbsp; <span class="blue">Bind</span> :: f a -> (a -> <span class="red">Free</span> f b) ->
      <span class="red">Free</span> f b
    </div>

    <p>
      This datatype allows us to lift primitive operations given by some signature
      <span>f :: <span class="red">Type</span> -> <span class="red">Type</span></span> into arbitrary monadic
      expressions. By using the following <span><span class="red">State</span> s a</span> datatype, for example, we can
      build functions that, for an appropriate handler, manipulate a state of type s. The second parameter of this
      datatype is the return type of its primitive operations.
    </p>

    <div class="code-line">
      <span class="purple">data</span> <span class="red">State</span> s a <span class="purple">where</span>
      <br>
      &nbsp; <span class="blue">Get</span> :: <span class="red">State</span> s s
      <br>
      &nbsp; <span class="blue">Put</span> :: s -> <span class="red">State</span> s ()
    </div>

    <p>
      There is growing interest in indexing monads by a <q>grading</q> that provides some abstraction of the operations
      they perform. This is no different from considering the length of the list as an abstraction over its value, and
      has the same benefits. However, we further require that a monad's grading is a monoid, so it has a sensible notion
      of composition and identity that mirrors the monad's structure.
    </p>

    <p>
      As we are concerned with the free monad a good place to start is with the free monoid, i.e. lists. Considering
      signatures that are themselves indexed by some type i, we can index monadic expressions as either the empty list
      in the case of <span class="blue">Pure</span>, indicating that it has no effect, or by prepending the index of a
      primitive operation, <span>f i a</span>, to those of the continuation, <span><span class="red">Free</span> f is
      b</span>, for the <span class="blue">Bind</span> constructor. Here the ticked constructors (e.g. '[]) merely
      indicate that we have promoted them to the type-level.
    </p>

    <div class="code-line">
      <span class="purple">type</span> <span class="red">Free</span> :: (i -> <span class="red">Type</span> ->
      <span class="red">Type</span>) -> [i] -> <span class="red">Type</span> -> <span class="red">Type</span>
      <br>
      <span class="purple">data</span> <span class="red">Free</span> f is a <span class="purple">where</span>
      <br>
      &nbsp; <span class="blue">Pure</span> :: a -> <span class="red">Free</span> f '[] a
      <br>
      &nbsp; <span class="blue">Bind</span> :: f i a -> (a -> <span class="red">Free</span> f is b) ->
      <span class="red">Free</span> f (i ': is) b
    </div>

    <p>
      We can then enrich our effect signature with an index that distinguishes between different sorts of primitive
      operation. In this case, to indicate whether it is a Get or Put. And specify that shape of an effectful operation
      at compile time!
    </p>

    <div class="code-line">
      <span class="purple">data</span> <span class="red">OpSort</span> <span class="purple">where</span>
      <br>
      &nbsp; <span class="blue">G</span> :: <span class="red">OpSort</span>
      <br>
      &nbsp; <span class="blue">P</span> :: <span class="red">OpSort</span>
      <br>
      <br>
      <span class="purple">type</span> <span class="red">State</span> :: <span class="red">Type</span> ->
      <span class="red">OpSort</span> -> <span class="red">Type</span> -> <span class="red">Type</span>
      <br>
      <span class="purple">data</span> <span class="red">State</span> s i a <span class="purple">where</span>
      <br>
      &nbsp; <span class="blue">Get</span> :: <span class="red">State</span> s 'G s
      <br>
      &nbsp; <span class="blue">Put</span> :: s -> <span class="red">State</span> s 'P ()
      <br>
      <br>
      modify :: <span class="red">Free</span> (<span class="red">State</span> s) '['<span class="blue">G</span>,
      '<span class="blue">P</span>] ()
      <br>
      modify = <span class="blue">Get</span> `<span class="blue">Bind</span>` (\s -> <span class="blue">Put</span> (s +
      1) `<span class="blue">Bind</span>` \() -> <span class="blue">Pure</span> ()))
    </div>

    <h2>Regular Effect Protocols</h2>

    <p>
      This technique for statically prescribing a sequence of effects can be thought of as a protocol that defines the
      acceptable behaviour of an operation. It is not, however, particularly useful because we have to know the
      <em>exact</em> sequence. What would be a lot nicer is if we were able to describe a set of valid behaviours. A
      convenient way of specifying a set of lists (i.e. a language) is as a regular expression. For example, if we
      require that all <span class="blue">Get</span> operations are followed by a <span class="blue">Put</span> we could
      write
      <span><span class="blue">Star</span> (<span class="blue">G</span> <span class="blue">:.</span>
      <span class="blue">P</span>)</span> using the following DSL:
    </p>

    <div class="code-line">
      <span class="purple">data</span> <span class="red">Regex</span> i
      <br> &nbsp; = <span class="green">-- The empty language.</span><br>
      &nbsp; &nbsp; <span class="blue">Empty</span>
      <br> &nbsp; | <span class="green">-- The language containing just the empty string.</span> <br>
      &nbsp; &nbsp; <span class="blue">Epsilon</span>
      <br> &nbsp; | <span class="green">-- The language containing a single character string.</span> <br>
      &nbsp; &nbsp; <span class="blue">Unit</span> i
      <br> &nbsp; | <span class="green">-- The sequential composition of languages.</span> <br>
      &nbsp; &nbsp; <span class="red">Regex</span> i <span class="blue">:.</span> <span class="red">Regex</span> i
      <br> &nbsp; | <span class="green">-- The union of languages.</span> <br>
      &nbsp; &nbsp; <span class="red">Regex</span> i <span class="blue">:||</span> <span class="red">Regex</span> i
      <br> &nbsp; | <span class="green">-- The intersection of languages.</span> <br>
      &nbsp; &nbsp; <span class="red">Regex</span> i <span class="blue">:&&</span> <span class="red">Regex</span> i
      <br>&nbsp; | <span class="green">-- The Kleene star (i.e. iteration) of a language.</span> <br>
      &nbsp; &nbsp; <span class="blue">Star</span> (<span class="red">Regex</span> i)
    </div>

    <p>
      A very satisfying property of regular expressions is that the language which remains after consuming a single
      character is another regular expression. Let's unpack this briefly with an example. If I start with the language
      <span><span class="blue">Star</span> (<span class="blue">G</span> <span class="blue">:.</span>
      <span class="blue">P</span>)</span> and observe a <span class="blue">G</span>, then the following sequence of
      characters must satisfy <span><span class="blue">Star</span> (<span class="blue">P</span>
      <span class="blue">:.</span> <span class="blue">G</span>)</span>. What happens if I observe a
      <span class="blue">P</span>? Well the remaining language is <span class="blue">Empty</span> as no string
      following a <span class="blue">P</span> will satisfy our protocol (not even the empty string
      <span class="blue">Epsilon</span>). This construction is called the Brzozowski derivative and will be key to
      ensuring the type of the overall tree, that is our protocol, can effectively dictate the type of the subtrees,
      which will be the derivative with respect to the operations encountered so far.
    </p>

    <p>
      The Brzozowski derivative is defined by two type families (i.e. type-level functions). The first
      <span class="red">Nu</span>, read <q>nullable</q>, checks whether the language of a regular expression contains
      the empty string. It returns '<span class="blue">Epsilon</span>, if this is the case, and
      '<span class="blue">Empty</span> otherwise. As we wish to minimise the amount of equational reasoning the compile
      has to do, and in fact, we won't specify any of the equational laws of regular expressions, this function also
      explicitly shortcuts the result via the <span class="red">Force</span> type family, oppose to the usual
      presentation where the output is a regular expression that is merely equivalent to
      '<span class="blue">Epsilon</span> or '<span class="blue">Empty</span> up-to some equations.
    </p>

    <div class="code-line">
      <span class="green">-- Check whether a regular expression permits the empty string.</span>
      <br>
      <span class="purple">type</span> <span class="red">Nu</span> :: <span class="red">Regex</span> i ->
      <span class="red">Regex</span> i
      <br>
      <span class="purple">type</span> <span class="purple">family</span> <span class="red">Nu</span> r
      <span class="purple">where</span>
      <br>
      &nbsp; <span class="red">Nu</span> '<span class="blue">Empty</span> = '<span class="blue">Empty</span>
      <br>
      &nbsp; <span class="red">Nu</span> '<span class="blue">Epsilon</span> = '<span class="blue">Epsilon</span>
      <br>
      &nbsp; <span class="red">Nu</span> ('<span class="blue">Unit</span> i) = '<span class="blue">Empty</span>
      <br>
      &nbsp; <span class="red">Nu</span> (r '<span class="blue">:.</span> s) = <span class="red">Force</span>
      (<span class="red">Nu</span> r '<span class="blue">:&&</span> <span class="red">Nu</span> s)
      <br>
      &nbsp; <span class="red">Nu</span> (r '<span class="blue">:&&</span> s) = <span class="red">Force</span>
      (<span class="red">Nu</span> r '<span class="blue">:&&</span> <span class="red">Nu</span> s)
      <br>
      &nbsp; <span class="red">Nu</span> (r '<span class="blue">:||</span> s) = <span class="red">Force</span>
      (<span class="red">Nu</span> r '<span class="blue">:||</span> <span class="red">Nu</span> s)
      <br>
      &nbsp; <span class="red">Nu</span> ('<span class="blue">Star</span> r) = '<span class="blue">Epsilon</span>
      <br>
      <br>
      <span class="green">-- Simplify the result.</span>
      <br>
      <span class="purple">type</span> <span class="red">Force</span> :: <span class="red">Regex</span> i ->
      <span class="red">Regex</span> i
      <br>
      <span class="purple">type</span> <span class="purple">family</span> <span class="red">Force</span> r
      <span class="purple">where</span>
      <br>
      &nbsp; <span class="red">Force</span> '<span class="blue">Empty</span> = '<span class="blue">Empty</span>
      <br>
      &nbsp; <span class="red">Force</span> '<span class="blue">Epsilon</span> = '<span class="blue">Epsilon</span>
      <br>
      &nbsp; <span class="red">Force</span> ('<span class="blue">Empty</span> '<span class="blue">:&&</span> _) =
      '<span class="blue">Empty</span>
      <br>
      &nbsp; <span class="red">Force</span> (_ '<span class="blue">:&&</span> '<span class="blue">Empty</span>) =
      '<span class="blue">Empty</span>
      <br>
      &nbsp; <span class="red">Force</span> ('<span class="blue">Epsilon</span> '<span class="blue">:&&</span> s) =
      <span class="red">Force</span> s
      <br>
      &nbsp; <span class="red">Force</span> (r '<span class="blue">:&&</span> '<span class="blue">Epsilon</span>) =
      <span class="red">Force</span> r
      <br>
      &nbsp; <span class="red">Force</span> ('<span class="blue">Epsilon</span> '<span class="blue">:||</span> _) =
      '<span class="blue">Epsilon</span>
      <br>
      &nbsp; <span class="red">Force</span> (_ '<span class="blue">:||</span> '<span class="blue">Epsilon</span>) =
      '<span class="blue">Epsilon</span>
      <br>
      &nbsp; <span class="red">Force</span> ('<span class="blue">Empty</span> '<span class="blue">:||</span> s) =
      <span class="red">Force</span> s
      <br>
      &nbsp; <span class="red">Force</span> (r '<span class="blue">:||</span> '<span class="blue">Empty</span>) =
      <span class="red">Force</span> r
      <br>
    </div>

    <p>
      The second type family, which defines the derivative operation, is <span class="red">Delta</span>. It takes two
      parameters: an observed operation i, and the regular expression r. The output is the language such that each
      string
      can be prepended with i to make a string from r.
    </p>

    <div class="code-line">
      <span class="green">-- The derivative of a regular expression.</span>
      <br>
      <span class="purple">type</span> <span class="red">Delta</span> :: i -> <span class="red">Regex</span> i ->
      <span class="red">Regex</span> i
      <br>
      <span class="purple">type</span> <span class="purple">family</span> <span class="red">Delta</span> i r
      <span class="purple">where</span>
      <br>
      &nbsp; <span class="red">Delta</span> i '<span class="blue">Empty</span> = '<span class="blue">Empty</span>
      <br>
      &nbsp; <span class="red">Delta</span> i '<span class="blue">Epsilon</span> = '<span class="blue">Empty</span>
      <br>
      &nbsp; <span class="red">Delta</span> i ('<span class="blue">Unit</span> i) = '<span class="blue">Epsilon</span>
      <br>
      &nbsp; <span class="green">-- Here we know that i and j are distinct operations.</span>
      <br>
      &nbsp; <span class="green">-- Otherwise, the previous case would have matched.</span>
      <br>
      &nbsp; <span class="red">Delta</span> i ('<span class="blue">Unit</span> j) = '<span class="blue">Empty</span>
      <br>
      &nbsp; <span class="green">-- Either i appears at the start of a string from r,</span>
      <br>
      &nbsp; <span class="green">-- or r contains the empty string and i appears in s.</span>
      <br>
      &nbsp; <span class="red">Delta</span> i (r '<span class="blue">:.</span> s) = (<span class="red">Delta</span> i r
      '<span class="blue">:.</span> s) '<span class="blue">:||</span> (<span class="red">Nu</span> r
      '<span class="blue">:.</span> <span class="red">Delta</span> i s)
      <br>
      &nbsp; <span class="red">Delta</span> i (r '<span class="blue">:&&</span> s) = <span class="red">Delta</span> i r
      '<span class="blue">:&&</span> <span class="red">Delta</span> i s
      <br>
      &nbsp; <span class="red">Delta</span> i (r '<span class="blue">:||</span> s) = <span class="red">Delta</span> i r
      '<span class="blue">:||</span> <span class="red">Delta</span> i s
      <br>
      &nbsp; <span class="red">Delta</span> i ('<span class="blue">Star</span> r) = <span class="red">Delta</span> i r
      '<span class="blue">:.</span> '<span class="blue">Star</span> r
    </div>

    <h2>Putting It Altogether!</h2>

    <p>
      Now we have an appropriate way of passing type information to subtrees, we can define a free monad indexed not by
      a list of operations but by a regular language of operations! In the case of <span class="blue">Pure</span>, we
      need to know that the empty string is accepted by the protocol. Luckily, the <span class="red">Nu</span>
      type family checks exactly that! For the <span class="blue">Bind</span> constructor, we just need to take the
      derivative of the target protocol with respect to the observed operation to find a suitable protocol for the
      continuation.
    </p>

    <div class="code-line">
      <span class="purple">type</span> <span class="red">Free</span> :: (i ->
      <span class="red">Type</span> ->
      <span class="red">Type</span>) -> <span class="red">Regex</span> i -> <span class="red">Type</span> -> <span class="red">Type</span>
      <br>
      <span class="purple">data</span> <span class="red">Free</span> f r a <span class="purple">where</span>
      <br>
      &nbsp; <span class="blue">Pure</span> :: <span class="red">Nu</span> r ~ '<span class="blue">Epsilon</span> => a
      -> <span class="red">Free</span> f r a
      <br>
      &nbsp; <span class="blue">Bind</span> :: f i a -> (a -> <span class="red">Free</span> f
      (<span class="red">Delta</span> i r) b) ->
      <span class="red">Free</span> f r b
      <br>
      <br>
      <span class="purple">type</span> <span class="red">GetPut</span> = '<span class="blue">Star</span>
      ('<span class="blue">G</span> '<span class="blue">:.</span> '<span class="blue">P</span>)
      <br>
      <br>
      <span class="green">-- This function <em>will</em> type check!</span>
      <br>
      safe :: <span class="red">Free</span> (<span class="red">State</span> <span class="red">Int</span>)
      <span class="red">GetPut</span> ()
      <br>
      safe = <span class="blue">Get</span> `<span class="blue">Bind</span>` (\s -> <span class="blue">Put</span> (s +
      1) `<span class="blue">Bind</span>` \() -> <span class="blue">Pure</span> ())
      <br>
      <br>
      <span class="green">-- This function <em>won't</em> type check!</span>
      <br>
      unsafe :: <span class="red">Free</span> (<span class="red">State</span> <span class="red">Int</span>)
      <span class="red">GetPut</span> ()
      <br>
      unsafe = <span class="blue">Get</span> `<span class="blue">Bind</span>` \s -> <span class="blue">Pure</span> ()
    </div>

    <p>
      This indexed monad is really quite neat because we can now statically verify that only one of these two functions
      actually meets our protocol. It is not a panacea though! We cannot, for example, directly compose monadic
      operations defined in this manner. Intuitively, this is because the type-checker is expecting to verify a protocol
      against a sequence of primitive operations, not another protocol. It does go to show, however, that organising
      structures along algebraic lines can serve not just the programmer but the compiler too.
    </p>
  </main>
</body>

</html>