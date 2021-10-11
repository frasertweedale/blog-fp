---
tags: security
---

# How to protect *aeson* code from hash flooding

A few weeks ago Tom Sydney Kerckhove ([\@kerckhove_ts][twitter-syd])
published [an excellent writeup][writeup] of a serious DoS
vulnerability in [*aeson*][hackage-aeson], a widely used Haskell
JSON library.  [A new *aeson* release addresses the hash flooding
issue, but you **need more than a version bump** to ensure your
programs are protected.]{.abstract}  This post outlines how *aeson*
addressed the vulnerability and what action *you* need to take.

[twitter-syd]: https://twitter.com/kerckhove_ts/
[writeup]: https://cs-syd.eu/posts/2021-09-11-json-vulnerability
[hackage-aeson]: https://hackage.haskell.org/package/aeson

## Overview of the issue

[Tom's article][writeup] is great and if you want the gory details,
go read it.  There's no need for me to repeat it here.  It's enough
to say that the attack, called *hash flooding* or *hash DoS*,
exploits the behaviour of the [`HashMap`][haddock-HashMap]
implementation from *unordered-containers*, which *aeson* used.  It
results in a denial of service through CPU consumption.  This
technique has been used in real-world attacks against a variety of
languages, libraries and frameworks over the years.

[hash table]: https://en.wikipedia.org/wiki/Hash_table
[haddock-HashMap]: https://hackage.haskell.org/package/unordered-containers-0.2.14.0/docs/Data-HashMap-Lazy.html

## Am I vulnerable?

If you are using `aeson < 2.0.0.0` and processing JSON from
untrusted sources, you are probably vulnerable.  You could mitigate
the attack by refusing to decode large inputs, if your use case
allows it.  Rate limiting may be a possible mitigation for some
applications.

## How did *aeson* address the vulnerability?

Whereas prior versions used `HashMap` directly, starting at version
`2.0.0.0` *aeson* abstracts the map implementation behind a new data
type: [`Data.Aeson.KeyMap`][haddock-KeyMap].  The `ordered-keymap`
Cabal flag selects the underlying implementation.  When set, *aeson*
uses the `Ord`-based [`Map`][haddock-Map] from *containers*.  If
unset, *aeson* uses [`HashMap`][haddock-HashMap].

[haddock-KeyMap]: https://hackage.haskell.org/package/aeson-2.0.1.0/docs/src/Data.Aeson.KeyMap.html
[haddock-Map]: https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#t:Map

Version `2.0.0.0` defaults the flag to `False`.  As of `2.0.1.0` it
defaults to `True`.  Importantly, the maintainers offer [**no
guarantee that the default won't change again**][no-guarantee].  So
if you use *aeson* and want to protect yourself from hash flooding
attacks, take the extra precautions outlined in the following
sections.

[no-guarantee]: https://github.com/haskell/aeson/issues/864#issuecomment-939363297

This is an API-breaking change, hence the major version bump.  Most
users will not have to change much code, but there will be
exceptions (I had to change quite a lot for [*jose*][hackage-jose]).

[hackage-jose]: https://hackage.haskell.org/package/jose 

The `Map` version also behaves differently from `HashMap`.  In
particular, objects may be serialised with a different key order,
and object keys are iterated in different orders.  And who knows
what systems out there depend on the key order in some way, even
though they should not.  That is a big reason why the maintainers
felt it was necessary to keep the option of using `HashMap`.

Also, these data structures have different performance
characteristics, with `Map` having *O(log n)* insertion and lookup
time.  `HashMap` insertion and lookup are amortised *O(1)*,
degrading to *O(n)* for pathological inputs—which is the cause of
the vulnerability!


## Compiling a safe version of aeson

If you have a program or library that uses *aeson*, you need to
ensure that the *aeson* you link against was compiled with the
`ordered-keymap` flag.  There is no way to express this condition in
a `.cabal` file, but you can *can* express these constraints in the
`cabal.project` file:

```
packages: .
constraints:
  aeson +ordered-keymap
```

For Stack users, configure the flag in your `stack.yaml`:

```
flags:
  aeson:
    ordered-keymap: true
```

If you're building and installing *aeson* directly, via
*cabal-install* (the `cabal` program), you can use the
`--flags=ordered-keymap` command line option.


## Runtime checks

In your program or library you can also detect the `KeyMap`
implementation at runtime.  If you detect `HashMap` you could abort,
emit a warning, or employ other mitigations like limiting the input
size.

`Data.Aeson.KeyMap` exports the following types:

```haskell
coercionToHashMap
  :: Maybe (Coercion (HashMap Key v) (KeyMap v))

coercionToMap
  :: Maybe (Coercion     (Map Key v) (KeyMap v))
```

The values are coercions—proofs of representational equality
enabling zero-cost conversions; see
[`Data.Type.Coercion`][haddock-Coercion].  Only one of `HashMap` or
`Map` is actually used, which is why they're wrapped in `Maybe`.
The map implementation that *aeson* is using has a non-`Nothing`
coercion.

[haddock-Coercion]: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Type-Coercion.html#t:Coercion

In [*jose*][hackage-jose] I will export the following value to make
it easy for library users to check that the implementation is safe
from hash flooding:

```haskell
vulnerableToHashFlood :: Bool
vulnerableToHashFlood =
  case KeyMap.coercionToMap of
    Just _  -> False
    Nothing -> True
```

Users can (and hopefully will) check that value and respond in
whatever way is suitable for their use case.  I might go even
further and cause all JWS processing to immediately fail when the
vulnerable implementation is detected, unless the caller overrides
this behaviour.


## What about other things that use `HashMap`?

The `HashMap` data structure from *unordered-containers* remains
vulnerable to hash flooding attacks.  Users and maintainers are
discussion potential solutions and mitigations in [issue #319][issue-u-c].
There are several interesting ideas, including:

- Initialise the library with a random salt, via `unsafePerformIO`.
  Many libraries in other language ecosystems use this approach.
  But it breaks referential integrity.  Values and orders will not
  be stable across different executions.

- Use a more collision-resistant hash algorithm, or multiple hashes,
  to make it harder to compute collisions.

- Don't do anything, because the other ideas come with performance
  or usability penalties.  If your program needs to be safe against
  hash flooding, employ other mitigations (size check, rate
  limiting, etc) or use an ordered map.

[issue-u-c]: https://github.com/haskell-unordered-containers/unordered-containers/issues/319

This discussion is ongoing.  The only change so far is to add a
security advisory to the package description.


## Conclusion

`aeson >= 2.0.0.0` has mitigated the hash flooding vulnerability.
Users of the library must take specific action not only to upgrade
*aeson* to the latest version, but also ensure it is compiled with
the correct flags.  Programs can also perform runtime checks and
take appropriate action if *aeson* is using `HashMap`.
