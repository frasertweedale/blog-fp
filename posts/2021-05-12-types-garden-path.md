---
tags: gotcha, testing
---

# Type-guided development and garden paths

*"Types help you reason about effects"*, we declare.  And they do!
Except when they don't.  *"Just follow the types!"* we insist.  But
sometimes the types take you down a garden path.

When the type checker is happy but the behaviour is all wrong, it
can be hard to find where you took the wrong turn.  In this post
I'll share real-world examples of this phenomenon, and offer some
tips on how to avoid it.

## Random generation of applicatives

The `Applicative` type class provides a function for lifting a
"pure" value into the applicative data type:

```haskell
class Applicative (k :: * -> *) where
  pure :: a -> k a
  (<*>) :: k (a -> b) -> k a -> k b
```

Assume we have a random generator of values of type `a`, and wish to
generate random applicatives.  The shape of this problem is:

```haskell
genAp :: (Applicative t) => Gen a -> Gen (t a)
```

We can generalise `Gen` to any effect `f`:

```haskell
effAp :: (Applicative t) => f a -> f (t a)
```

How can implement this?  *Follow the types!*  `t` has an
`Applicative` instance, so if we introduce a `Functor` constraint on
`f` we can write:

```haskell
effAp :: (Functor f, Applicative t) => f a -> f (t a)
effAp = fmap pure
```

Now we have a nice, general function that runs an effect and lifts
the result into an applicative.  Let's test it by generating
single-value lists of `Word8`:

```haskell
λ> effAp randomIO :: IO [Word8]
[120]
λ> effAp randomIO :: IO [Word8]
[33]
```

OK!  Now let's use it for the following vector type:

```haskell
data V3 a = V3 a a a

-- boring
instance Functor V3
instance Applicative V3
instance Foldable V3
instance Traversable V3
```

::: note

This type is similar to [`Linear.V3.V3`][linear-v3-doc] from
[*linear*][linear-hackage], a popular linear algebra package.

:::

[linear-v3-doc]: https://hackage.haskell.org/package/linear-1.21.5/docs/Linear-V3.html#t:V3
[linear-hackage]: https://hackage.haskell.org/package/linear

The random `V3` values generated are:

```haskell
λ> effAp randomIO :: IO (V3 Word8)
V3 186 186 186
λ> effAp randomIO :: IO (V3 Word8)
V3 215 215 215
```

Oh dear.  We followed the types to implement `effAp`, but the
implementation is not correct!  Instead of running the effect 3
times to generate 3 random vector components, it ran the effect once
and used the result 3 times.

`effAp` should first lift the *effect* into the applicative type
using `pure`, giving a value of the type `(Applicative t) => t (f
a)`.  The shape of the hole is now `t (f a) -> f (t a)`.  That is
exactly the shape of `sequenceA`:

```haskell
sequenceA
    :: (Traversable t, Applicative f)
    => t (f a) -> f (t a)
```

Accepting the tighter constraints, the implementation of `effAp`
becomes:

```haskell
effAp
    :: (Traversable t, Applicative f, Applicative t)
    => f a -> f (t a)
effAp = sequenceA . pure
```

Now `effAp` has the expected behaviour:

```
λ> effAp randomIO :: IO (V3 Word8)
V3 251 198 213
```

::: note

`V3` is one of many types for which `fmap pure` and `sequenceA .
pure` behave differently.  Other examples include [`Join
(,)`][join-doc] and [`Proxy`][proxy-doc].

:::

[join-doc]: https://hackage.haskell.org/package/bifunctors-5.5.11/docs/Data-Bifunctor-Join.html#t:Join
[proxy-doc]: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Proxy.html#t:Proxy


## Composing effects (by ignoring them)

In [Purebred][] we have an input validation system that checks
inputs as the user types.  It dispatches the validation work to a
background thread, so the UI stays responsive.  Each time the user
edits the input, the program kills the outstanding validation thread
(if any) and spawns a new one.  The program code (simplified for
this article) is:

[Purebred]: https://github.com/purebred-mua/purebred

```haskell
dispatchValidation = do
  let spawn = forkIO doValidation
  oldId <- getValidationThread
  newId <- maybe spawn (killThread $> spawn) oldId
  _ <- setValidationThread (Just newId)
```

The outstanding validation thread is stored in a `Maybe ThreadId`.
In the `Just` case the program kills the old thread, spawns a new
thread and returns the new `ThreadId`:

```haskell
(killThread $> spawn) :: ThreadId -> IO ThreadId
```

Except, it does not.  At a glance, we see the actions occurring in
the correct order.  But there was a bug.  Input validation had
unexpected and nondeterministic results.  For example, a valid input
might (or might not) result in an error being shown.

We can apprehend the error by equational reasoning:

```haskell
  killThread $> spawn
= const spawn <$> killThread       -- definition of ($>)
= const spawn . killThread         -- fmap for ((->) r)
= \x -> const spawn (killThread x) -- definition of (.)
= \x -> spawn                      -- definition of const
```

The expression discards the old `ThreadId` and never executes
`killThread`.  As a consequence, validation threads run wild,
finishing their work in a nondeterminstic order.

When we finally understood the problem, [the fix][] was
straightforward.  We replaced the expression:

```haskell
killThread $> schedule  -- broken
```

with:

```haskell
\t -> killThread t *> schedule  -- fixed
```

[the fix]: https://github.com/purebred-mua/purebred/pull/413/commits/4eefd939d4bb201c37e5fe2956e8777e85a6b930

## Lessons learned

I discussed two bugs where the type checker was happy and the code
seemed *superficially* reasonable.  Both had implementations guided
by "type tetris" (*what fits here?*) that turned out to be
fundamentally wrong.  What strategies can help avoid such traps?

The first bug involved use of the wrong abstraction; `Functor`
instead of `Applicative`.  Making an effort to test generic
functions with a greater diversity of instances might have helped
discover this bug sooner.  In the case of applicatives, don't just
test with `[]`, `Maybe` and other "common" types where `pure`
constructs a "singleton" value.  Test with [`Proxy a`][proxy-doc]
(which has zero `a` values), [`Join (,) a`][join-doc] (two `a`
values), and so on.

As for the second bug, my advice is don't try to be clever when
writing effectful code.  `do` notation and explicit binds are fine.
Using underscore binds to ignore values may result in more readable
code than `const` and related functions.  If you have a hole with a
function type, start by writing the lambda, and work step by step to
complete the definition.  Make sure it's correct first, and only
then tidy it up (if you want to).

In the case of the Purebred bug, we had a hole with the type
`ThreadId -> IO ThreadId`, and filled it with an expression that
**subtly** ignored the `ThreadId` argument.  Our next step, when
faced with this hole, should have been to write the expression
`(\threadId -> _)`.  Critically, this binds the `ThreadId` argument,
making it hard to ignore.

The final takeaway is: don't be smug about the power of the type
system.  Type-guided development is indeed wonderful and powerful,
but let's be honest that in many cases, reasoning from the types
alone won't get you all the way to a correct solution.  On the
contrary, you may find yourself at the end of a garden path!
