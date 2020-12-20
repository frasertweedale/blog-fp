---
tags: refactoring, optics
---

# Refactoring using type classes and optics

Often when writing programs and functions, one starts off with
concrete types that solve the problem at hand.  At some later time,
due to emerging requirements or observed patterns, or just to
improve code readability and reusability, we refactor to make the
code more polymorphic.  The importance of not breaking your API
typically ranges from *nice to have* (e.g. minimise rework but not
otherwise necessary) to *paramount* (e.g. in a popular, foundational
library).  This post is a case study of a refactoring in the
[*jose*][jose] library demonstrating how *type classes* help achieve
API stability while admitting new use cases.  Served with a side of
*classy optics*.

[jose]: https://hackage.haskell.org/package/jose


## Background: verifying a JWS

In the *jose* library, the `verifyJWS` function verifies a *JSON
Web Signature (JWS)* object:

```haskell
verifyJWS
  ::  ( HasAlgorithms a, HasValidationPolicy a
      , AsError e, MonadError e m
      , HasJWSHeader h, HasParams h
      )
  => a
  -> JWK
  -> JWS h
  -> m ()
```

`verifyJWS` is applied to a configuration value, a *JSON Web Key
(JWK)* to use for validation, and the JWS, and returns `()` on
success otherwise throws an error.

A JWS can have multiple signatures, each by a different key.  If an
application requires all signatures to be valid, it is difficult to
perform the correct validation using the existing `verifyJWS`
function.  Unsurprisingly, someone [raised an issue][issue] to
address this.  Specifically, the issue asked for a way to use a *JWK
Set* instead of a single *JWK*, where the JWK Set contains all the
keys that can be used for validation (and possibly others).  JWK Set
is [defined by RFC 7515][JWKSet] as an array of JWKs.  Its
definition in *jose* is below (it is a `newtype` because it needs
custom instances for JSON encoding/decoding).

```haskell
newtype JWKSet = JWKSet [JWK]
```

[issue]: https://github.com/frasertweedale/hs-jose/issues/35
[JWKSet]: https://tools.ietf.org/html/rfc7517#section-5

How can we support verification when the caller has a `JWKSet`?  Do
we add an alternative verification function that applies to `JWKSet`
instead of `JWK`?  I did not want multiple functions in the API for
essentially the same thing.  We could make the caller construct a
singleton `JWKSet`, but changing the signature of `verifyJWS`
would break the API, so we won't do that either.


## Looking beyond JWK Set

At first, we could only validate with a single `JWK` at a time.
Now, we want to be able to handle a `JWKSet` too.  So are there
other "key-bearing" structures we might need to handle?

Indeed there are.  One might want to use a `[JWK]`, `NonEmpty JWK`
or other container types.  Many container types constructors have an
instance of [`Foldable`][Foldable], so we could try to use that
abstraction.  This is rather boring, but it's an additional valid
use case.  So, can we refactor `verifyJWS` to use `Foldable`?

[Foldable]: https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Foldable.html

It is a nice idea, but the answer is *no*.  First, `Foldable t => t`
has kind `(* -> *)`, but `JWKSet` has kind `*`.  Turning `JWKSet`
into a generic container doesn't make sense either.  Second, we
still want our function to work with a plain old JWK, which also has
kind `*`.

At this point I realised that that if I want to make `verifyJWS`
polymorphic, *and* support `JWK` and `JWKSet` and arbitrary
containers as "key sources", *and* avoid breaking the API, there was
only one way forward.  It was necessary to define a new type class
to represent a "provider of keys":

```haskell
class JWKStore a where
  keys :: ???
```

What should the type of `keys` be?  Conceptually, it would be
applied to a `JWKStore a => a` and yield the `JWK` values contained
within.  For the validation use case there is no need to be able to
add or update keys, i.e. it is read-only.  `Fold a JWK` is a good
fit for the requirement.  A [`Fold`][Fold] is a read-only *optic*
that that can retrieve multiple (zero or more) values.  The beauty
of optics is that they can be composed together and `Fold` is no
different.  The `Fold` type is provided by the [*lens*][lens]
library, along with a bunch of useful helper functions.

[Fold]: https://hackage.haskell.org/package/lens-4.15/docs/Control-Lens-Fold.html
[lens]: https://hackage.haskell.org/package/lens

The key store interface (type class) and instances were defined as
follows:

```haskell
class JWKStore a where
  keys :: Fold a JWK

instance JWKStore JWK where
  keys = id

instance Foldable t => JWKStore (t JWK) where
  keys = folding id

instance JWKStore JWKSet where
  keys = folding (\(JWKSet xs) -> xs)
```

(Note that the instance for `Foldable t => t JWK` requires the
[`FlexibleInstances`][FlexibleInstances] GHC extension.)

[FlexibleInstances]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleInstances

The updated type signature for `verifyJWS`:

```haskell
verifyJWS
  ::  ( HasAlgorithms a, HasValidationPolicy a
      , AsError e, MonadError e m
      , HasJWSHeader h, HasParams h
      , JWKStore k
      )
  => a
  -> k
  -> JWS h
  -> m ()
```

Existing code applying `verifyJWS` to a `JWK` works without
changes.  The only internal change that was needed was to apply
`anyOf keys` to the existing test.  ([`anyOf`][anyOf] is a function
provided in *lens* that returns `True` if any target of a `Fold`
satisfies a predicate.)  The line:

[anyOf]: https://hackage.haskell.org/package/lens-4.19.2/docs/Control-Lens-Fold.html#v:anyOf

```haskell
validate s = verifySig p s k == Right True
```

became:

```haskell
validate s = anyOf keys ((== Right True) . verifySig p s) k
```

The technique of using type classes to select optics is called
*classy optics*.  The terms *classy lenses* and *classy prisms* are
also used when referring to those kinds of optics specifically.

My goal was to support validation using a `JWKSet` without breaking
the API or adding more functions.  At this point the goal has been
achieved.  But I mentioned above that `JWKSet` and `Foldable t => t
JWK` were the boring use cases.  Let's discuss some *interesting*
ones!


## Efficient key lookup

JWS signatures each have a *header* which, at minimum, indicates the
algorithm used (the `"alg"` member).  It can optionally contain
other data including a Key ID (`"kid"`), thumbprint of an X.509
certificate containing the key used make the signature
(`"x5t@S256"`), a JWK for the signing key (`"jwk"`), and so on.  It
is not a stretch that if your use case involves many signing keys,
you would want to use data available from the signature header to
speed up key lookup.  Such techniques are common in applied
cryptography where many keys are involved.  For example, X.509
certificates contain an *Authority Key Identifier* field, and
certificate databases usually provide efficient lookup by key
identifier.

So in addition to being able to enumerate keys, we want `JWKStore`
instances to potentially be able to look up keys based on data in
the JWS header.  I added another function to the type class to
accomplish this, along with a sensible default implementation:

```haskell
class JWKStore a where
  keys :: Fold a JWK

  keysFor :: (HasJWSHeader h) => h -> Fold a JWK
  keysFor _ = keys
```

As an example, we can instantiate `JWKStore a => a` at a data type
based on `HashMap`, where keys are indexed by key identifier (an
arbitrary string).  The `keysFor` function can efficiently search
for a `JWK` based on the `"kid"` (Key ID) header parameter from the
JWS header.  If the `"kid"` parameter is missing, it yields all
the `keys`.

```haskell
newtype KidMap = KidMap { getMap :: HashMap String JWK }

instance JWKStore KidMap where
  keys = folding getMap

  keyFor h = case preview (kid . _Just . param) h of
    Just k  -> folding (lookup k . getMap)
    Nothing -> keys
```


## A `JWKStore` is not just for JWS

Recall the type of `keysFor`:

```haskell
keysFor :: (HasJWSHeader h) => h -> Fold a JWK
```

`h` has an explicit `HasJWSHeader` type class constraint, which
allows the implementation to use any information available via that
type class to decide what to do.  For JWS we're basically done, but
we have forgotten about *JSON Web Encryption (JWE)*.  The concept of
looking up keys in a key database applies to JWE as well as JWS, but
the `HasJWSHeader` constraint is not suitable when you have a *JWE
header*.

But JWE headers and JWS headers consist of mostly the same fields,
with the same types and semantics.  So instead of having a type
class constraint mentioning the kind of header, we can define a type
class for every shared header parameter and constrain `keysFor` to
*all of them*.  I will use classy optics again ([lenses][Lens] this
time).  There are eleven header fields shared by JWS and JWE
headers, but for brevity I'll pretend there are only three.

[Lens]: https://hackage.haskell.org/package/lens-4.19.2/docs/Control-Lens-Lens.html

```haskell
class HasAlg a where
  alg :: Lens' a (HeaderParam JWA.JWS.Alg)

class HasKid a where
  kid :: Lens' a (Maybe (HeaderParam String))

class HasX5tS256 a where
  x5tS256 :: Lens' a (Maybe (HeaderParam Types.Base64SHA256))
```

The class definitions are mundane, as are the instances for
`JWSHeader`:

```haskell
instance HasAlg JWSHeader where
  alg f h@(JWSHeader { _jwsHeaderAlg = a }) =
    fmap (\a' -> h { _jwsHeaderAlg = a' }) (f a)

instance HasKid JWSHeader where
  kid f h@(JWSHeader { _jwsHeaderKid = a }) =
    fmap (\a' -> h { _jwsHeaderKid = a' }) (f a)

instance HasX5tS256 JWSHeader where
  x5tS256 f h@(JWSHeader { _jwsHeaderX5tS256 = a }) =
    fmap (\a' -> h { _jwsHeaderX5tS256 = a' }) (f a)
```

And finally, the updated `keysFor` type signature:

```haskell
class JWKStore a where
  keys :: Fold a JWK

  keysFor
    :: (HasAlg h, HasKid h, HasX5tS256)
    => h
    -> Fold a JWK
  keysFor _ = keys
```


## Further development

I mentioned that the refactor I outlined in this post was not the
end of the story.  In fact, much more was done since I performed
this initial refactoring:

- Implementing an [effectful key store][], so that key lookup can
  perform I/O.

- Allow the key store to [inspect the JWS payload][] during lookup.
  The motivating use case is to allow key lookup to read the JWT
  `"iss"` (Issuer) field.

- Parameterised the key store [over the header type][].  This allows
  implementations to use data from extended header types during key
  lookup.

[effectful key store]: https://github.com/frasertweedale/hs-jose/commit/e15ac3a34cf4a2f193f4b68345d8839c791ec2ed
[inspect the JWS payload]: https://github.com/frasertweedale/hs-jose/commit/dedcd82787bb533885211c3e7ff8f8bcd4adc3c9
[over the header type]: https://github.com/frasertweedale/hs-jose/commit/2fc1b0b8e7196f90ce68bae93fde591812dd3f22

All of these features involved adding type parameters to the
`JWKStore` class.  Again, I achieved **backwards compatible**
admittance of new features through increased generality.

However, parameterisation over the header type did make my use of
classy lenses (as described in this post) obsolete.  It is always
beneficial to define classy lenses and use them to document (and
restrict) functions that access specific fields of "types that have
those fields".  But if the type is even more general than that,
there's nothing more to do. 

Finally, implementing payload inspection was incompatible with
having a unified key store interface for both decryption and
verification keys.  (You can't use the payload to help choose a
decryption key becaues it is encrypted!)  Therefore I [renamed
`JWKStore` to `VerificationKeyStore`][rename].

[rename]: https://github.com/frasertweedale/hs-jose/commit/c08431fb31fb2a0bae86f6c4a5af20428e681230


## Conclusion

Let's recap what this post was all about and draw some conclusions.

First, I discussed a requirement to generalise the [*jose*][jose]
library's JWS validation code.  Previously, validation worked only
with a single `JWK`.  It needed to handle other use cases like
`JWKSet` or key databases.  I defined the `JWKStore` **type class**,
which provides access to JWKs inside arbitrary data types, and
refactored `verifyJWS` to use it.  Instances for `JWK` and `JWKSet`
are included.  The refactor was a **backwards-compatible
generalisation** of `verifyJWS` function, so existing code using
*jose* continued to work without changes.

After this, I added another function to `JWKStore` to allow
instances to support efficient key lookup.  Finally, I observed that
key databases are needed for JWE as well as JWS, and further
generalised `JWKStore` to account for this.

**Classy optics** were an important part of the implementation
resulting from this refactoring effort.  They were employed in two
different ways.  First, a [`Fold`][Fold] provides a read-only,
composable interface to the keys in a `JWKStore`.  Second, I used
classy [lenses][Lens] to parameterise key lookup over the fields
that are common to both JWS and JWE headers.  Classy lenses provide
the generality we desire *and* improve readability by documenting
the data that can be used during key lookup (and enforcing these
restrictions).

Finally I briefly discussed subsequent developments of the key store
interface in *jose*.  The important thing to note is that
substantive **backwards-compatible** enhancements were achieved
through **more generality** (polymorphism).

So here are some final take-aways:

- Starting with code that handles a single use case is normal.  But
  you may have to revisit and change that code (especially in
  libraries).

- When you have to revisit some code to admit more use cases, it is
  worth having a good think about what other use cases exist.
  Discovering a more general (polymorphic) interface is preferable
  to multiple concrete interfaces.  There will be **less code** to
  document and maintain, **less potential confusion** for users, and
  more potential for **reuse** (more use cases admitted).

- Introducing a **type class** may let you admit new use cases while
  preserving backwards compatibility for library users.  Further
  parameterising an existing type class can also achieve this.  I
  have done this many times (in *jose* and other projects) and have
  *never* regretted introducing another type parameter.

- **Classy optics** enable reuse; more types can be used with the
  function, including types you didn't even know about.  And they
  document and enforce the data a function can access, which is
  helpful when dealing with record types.
