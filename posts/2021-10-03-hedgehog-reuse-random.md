---
tags: testing
---

# Reusing random generators in Hedgehog

[Hedgehog][] has a powerful API for generating arbitrary values of
your types.  But sometimes a library will already provide a random
generator.  [In this post I show how to use existing generators with
Hedgehog, and discuss the advantages and disadvantages.]{.abstract}

[Hedgehog]: https://hedgehog.qa/

## Random generator use cases

Libraries may need to provide random generators of (some of) their
types for a variety of reasons.  Cryptographic keys, secrets and
unique identifiers come to mind immediately.

One use case we have in [*purebred-email*][hackage-purebred-email]
is generation of MIME multipart boundary values ([RFC
2046][rfc-2046]).  The boundary is a string with 1–70 characters
from a restricted alphabet.  Using a random boundary is useful
because the boundary delimiter line (the boundary value preceded by
two hyphens) must not appear anywhere within the message parts.

[rfc-2046]: https://www.rfc-editor.org/rfc/rfc2046.html#section-5.1

The `Boundary` type is defined as follows:

```haskell
-- constructor NOT exported
newtype Boundary = Boundary ByteString
  deriving (Eq, Show)

unBoundary :: Boundary -> ByteString
unBoundary (Boundary s) = s

-- smart constructor; checks length and validity
makeBoundary :: ByteString -> Either ByteString Boundary
```

We don't export the constructor.  Users must use the `makeBoundary`
*smart constructor* which checks that the input is a valid boundary
value.

We also instance the [`Uniform`][haddock-Uniform] type class from
the [*random*][hackage-random] package (version 1.2.0 onwards).
This instance provides a convenient way for users to generate
conformant boundary values that have a negligible probability of
matching any line in an arbitrary message.

```haskell
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as C8

instance Uniform Boundary where
  uniformM :: StatefulGen g m => g -> m a
  uniformM g =
    Boundary . B.unsafePackLenBytes 64 <$> randString
  where
    randString  = replicateM 64 randChar
    randChar    = B.index bchars <$> randIndex
    randIndex   = uniformRM (0, B.length bchars - 1) g
    bchars      = C8.pack $
                       ['a'..'z'] <> ['A'..'Z']
                    <> ['0'..'9'] <> "'()+_,-./:=?"
```

::: note

A `Uniform` instance is supposed to draw from all possible values of
a type.  In the `Boundary` instance we are only generating values of
length 64.  This is acceptable for our use case but may surprise
some users.

:::

The *random* library provides a very general interface to
instantiate and use random number generators.  I cannot cover it in
any detail in this post.  Assuming you already have a generator
value, [`System.Random.uniform`][haddock-uniform] generates a value
of any type with an instance of `Uniform`:

```haskell
uniform :: (RandomGen g, Uniform a) => g -> (a, g)
```

[haddock-uniform]: https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html#v:uniform

You can use `uniform` with
[`System.Random.getStdRandom`][haddock-getStdRandom] to generate
values using a global pseudo-random number generated initialised
from system entropy, as an `IO` action:

[haddock-getStdRandom]: https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html#v:getStdRandom

```haskell
getStdRandom :: MonadIO m => (StdGen -> (a, StdGen)) ->  m a
getStdRandom ::              (StdGen -> (a, StdGen)) -> IO a

getStdRandom uniform :: (MonadIO m, Uniform a) =>  m a
getStdRandom uniform ::            (Uniform a) => IO a
```

[hackage-purebred-email]: https://hackage.haskell.org/package/purebred-email
[haddock-Uniform]: https://hackage.haskell.org/package/random-1.2.0/docs/System-Random-Stateful.html#t:Uniform
[hackage-random]: https://hackage.haskell.org/package/random


## Hedgehog and hidden constructors

If a module does not expose the constructor of some type, how can
the test suite generate random values of that type?  There are
several ways you could tackle this:

1. Export the constructor from some "internal" module, which is not
   really internal.  In this way, library users may be
   discouraged—but not prevented—from constructing bad data.  The
   test module can import the constructor from the library's
   "internal" module and use it to define the generator.

2. Export a Hedgehog `Gen` for the type from the library itself.
   This causes the library to depend on Hedgehog, which is usually
   not desirable.

3. For a `newtype`, use
   [`Unsafe.Coerce.unsafeCoerce`][haddock-unsafeCoerce] in the `Gen`
   definition to coerce the underlying type to the wrapped type.
   You cannot use [`Data.Coerce.coerce`][haddock-coerce] if the
   constructor is not in scope.  This is nasty, but not unspeakable
   given we're talking about generators for the test suite.

[haddock-unsafeCoerce]: https://hackage.haskell.org/package/base-4.15.0.0/docs/Unsafe-Coerce.html
[haddock-coerce]: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Coerce.html

4. Export a "lightweight" random generator from the library, and
   reuse it to define the `Gen` in the test suite.  If you were
   going to export a `Uniform` (or `UniformRange`) instance anyway,
   this will be low-effort.  This approach is the main topic of this
   article.


## Implementing `Gen` using `Uniform`

I was aware that Hedgehog depends on *random*, and was hopeful of
finding a way to use the existing `Uniform` instance to implement a
`Gen Boundary`.  Looking through the docs, I stumbled across
[`generate`][haddock-generate]:

```haskell
generate :: MonadGen m => (Size -> Seed -> a) -> m a
```

It was not immediately apparent whether I could use `generate` to
define a `Gen Boundary`.  First, does `Gen` have an instance of
`MonadGen`?

```haskell
type Gen = GenT Identity

Monad m => MonadGen (GenT m)
```

Yes, it does.  Next, I had to work out how to turn a `Size` and a
`Seed` into a `Boundary`.  To my delight, I saw that `Seed` has an
instance of `RandomGen`.  Putting it together, all that is required
is to apply `uniform` to the `Seed`, and discard the new generator
value.  I ignore the `Size`.

```haskell
import Hedgehog (Gen)
import Hedgehog.Internal.Gen (generate)

genBoundary :: Gen Boundary
genBoundary = generate (\_size seed -> fst (uniform seed))
```

[haddock-generate]: https://hackage.haskell.org/package/hedgehog-1.0.5/docs/Hedgehog-Internal-Gen.html#v:generate


## Disadvantages

There are a few disadvantages to reusing a library's random
generator to define your Hedgehog `Gen`.

First, the generated values are restricted to whatever the library's
generator gives you.  In my case, the `Boundary` generator only
generates values of length 64.  It follows that Hedgehog could miss
all kinds of bugs.  For example, if *purebred-email* fails to decode
boundaries of length 70 due to an off-by-one error, I have no hope
of catching that bug.

Second, `generate` gives you a `Gen` with no shrinks.  If Hedgehog
finds a counterexample, it can't do anything to try and simplify it.
Automatic shrinking is one of Hedgehog'ss killer features, but you
give it up by using `generate`.

You can use the `shrink` function to supply additional shrinking
behaviour to a `Gen`:

```haskell
shrink :: MonadGen m => (a -> [a]) -> m a -> m a 
```

But when you don't have access to the constructor for the data type
you're generating, defining your own shrinks is at best awkward, and
maybe impossible.  I *could* implement `Boundary` shrinking by
extracting the underlying `ByteString` (`unBoundary`), shrinking it,
applying the smart constructor (`makeBoundary`) and filtering
invalid values.  That's a lot of work.  I didn't bother.

## Conclusion

Defining Hedgehog `Gen` values can be awkward or very difficult for
types whose constructors are hidden.  But if you have a function
that uses a `RandomGen` to generate values, you can use it with
Hedgehog's `generate` function to define a `Gen`.  The downsides are
that you don't get automatic shrinking, and you are restricted to
whatever values the generator produces.

Alternative approaches include exposing the constructor via an
"internal" (but actually public) module, or using `unsafeCoerce`.
