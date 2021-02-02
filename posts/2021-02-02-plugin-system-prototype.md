---
tags: plugins
---

# Prototyping a plugin system for Purebred

In this post I present an experimental design for a plugin system,
where plugins' capabilities are expressed in their types.  The types
are user-visible and human-friendly.  This is achieved without
sacrificing composability—plugins with different capabilities can be
composed together.

## Introduction

I'm working on a plugin system for [Purebred][], a mail user agent
program.  The goal of every plugin system is to enable smooth
integration (composition) of components from various packages into
the main program, providing additional or alternative behaviour.

[Purebred]: https://github.com/purebred-mua/purebred

My secondary goal for *this* plugin system is to express as much
information as possible about plugin behaviour *through types*.  A
plugin that purports to perform a pure computation (e.g. adding a
`User-Agent` header to outgoing mail) should not be allowed to
launch the missiles!  Its type should express this constraint.
Doing so communicates important information to the compiler (which
enforces the constraints), and to humans (who will ask questions
like, *"How safe is this plugin?"*)

Haskell provides for substantial and satisfying progress toward this
objective, without compromising the primary goal of composition.
After experimenting with several different approaches I arrived at
the design that I am presenting here in this post.  I will begin
with a description of the [test-bed program](#test-bed-program),
then outline my [requirements](#plugin-system-requirements) for a
plugin system.  Then I describe the [solution](#solution), and
conclude with a [discussion](#discussion) of design tradeoffs and
considerations for implementing this design in Purebred.


## Test-bed program

I wrote a small, simple program to serve as the test-bed for plugin
system experimentation.  This program reads a `Bool` and an `Int`
from user input, negates the number if the `Bool` is `True`, and
prints the result.

`Main.hs`:

```haskell
import System.IO (hFlush, stdout)
import Control.Monad.State

main :: IO ()
main = do
  -- read start values
  doNegate <- prompt "negate number? [True|False]"
  i <- prompt "number (Int)"

  (j, doNegate') <- flip runStateT doNegate $
    pure i  -- TODO run plugins

  -- print result
  let r = if doNegate' then negate j else j
  putStr "result: " *> print r

  where
  prompt s = putStr (s <> ": ") *> hFlush stdout *> readLn
```

Running the program, telling it to negate the input and giving it
the value `10`, gives the following transcript:

```
ftweedal% ./Main
negate number? [True|False]: True
number (Int): 10
result: -10
```

I put a `TODO` where I want to execute plugin behaviour, in a
`StateT Bool IO Int` computation.  Plugins should be able to
manipulate the `Int` *result* value, and/or read or modify the
`Bool` *state* value, and/or perform I/O.


## Plugin system requirements

The requirements for my plugin system are:

- **Composition**: Plugins from different modules or packages must
  be able to be used together in the main program.

- **Capabilities**: Plugins modules must express required
  capabilities in their exported type(s).  That is, the capabilities
  are visible to users, without reading source code.

- **Human-friendly**: The plugin system must be comprehensible to
  end-users, who are not necessarily programmers.

- **Configuration**: Some plugins will need user-specified
  configuration.  Static configuration (i.e.  no change after
  program initialisation) is acceptable.

- **Abstract**: Plugin behaviour types must not mention the type of
  the main program's monad transformer stack.  This allows the main
  program's implementation to evolve without breaking the plugin
  API.

### Not in scope

There are some other points in the plugin system design space that I
am not trying to solve:

- **Plugin loading**: How does the program find and load plugins?
  In my experimental design, the plugins will be statically compiled
  into the program.  This approach will also work for Purebred,
  because we use [*Dyre*][dyre] for configuration by recompilation
  (in the style of [*xmonad*][xmonad]).  Dynamic
  loading/unloading/reloading of plugins is an interesting topic,
  but I'm not trying to solve it here.  The [*Plugging Haskell
  In*][plugins-paper] paper (Chakravarty *et al*) and [*plugins*
  library][plugins-library] present one solution to the problem.

- **Plugin state**: What if a plugin needs to store/retrieve data
  *related to itself* in the program state?  Plugins have no control
  over the state data type of the main program.  I don't see a need
  for this in Purebred (yet), so I'm not going to tackle this
  problem.  But if I were, I would look at [`Data.Dynamic`][Dynamic]
  and [`Data.IORef`][IORef] as possible starting points.

- **Plugin "idempotency"**: If you enable the PGP plugin twice, will
  it sign/encrypt the outgoing message twice?  In the Purebred
  approach to configuration this sort of mistake is unlikely, so I'm
  going to ignore this problem.

[dyre]: https://hackage.haskell.org/package/dyre
[xmonad]: https://xmonad.org/
[plugins-paper]: https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.128.6930&rep=rep1&type=pdf
[plugins-library]: https://hackage.haskell.org/package/plugins
[Dynamic]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Dynamic.html
[IORef]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-IORef.html


## Solution

In this section I describe the implementation of the prototype
plugin system.  The code is also available in a [Git
repository][repo], under the [CC0][] license.  You can clone,
review, compile, test and experiment with the code yourself.

[repo]: https://github.com/frasertweedale/hs-plugin-system-prototype
[CC0]: https://creativecommons.org/publicdomain/zero/1.0/

### Language extensions

I define the plugin types and helpers in `Plugin.hs`.  Some language
extensions are required:

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Plugin where
```

These extensions are uncontroversial.  Only `UndecidableInstances`
is a bit iffy, but it's safe to use here.  I shall indicate the
declarations that require these extensions as we encounter them.

`Main.hs` and the modules defining the plugins themselves do not
require any language extensions.


### The `Plugin` type

The test-bed program will execute plugins in the `StateT Bool IO`
monad transformer stack.  Each plugin takes an `Int` input and
returns a (possibly modified) `Int`.  The concrete type is of such a
function is:

```haskell
type Plugin = Int -> StateT Bool IO Int
```

One of the requirements is that the plugin function type must be
abstract, so that the main program's monad transformer stack can
evolve if needed.  We also know that different plugins will have
different constraints, and that we want to express the constraints
in the type.  So let's parameterise the function type over the
constraint:

```haskell
type Plugin ctx = forall m. (ctx m) => Int -> m Int
```

That declaration requires the `RankNTypes` and `ConstraintKinds`
language extensions.  To understand `ConstraintKinds`, let's look at
the *kind* of `Plugin`:

```haskell
λ> :k Plugin
Plugin :: ((Type -> Type) -> Constraint) -> Type
```

`Constraint` is the kind of constraints (things that go before `=>`
in a type declaration).  The `Plugin` type constructor has a single
argument of kind `(Type -> Type) -> Constraint`.  Here are some
things that have that kind:

```haskell
Applicative     :: (Type -> Type) -> Constraint
MonadIO         :: (Type -> Type) -> Constraint
MonadState Bool :: (Type -> Type) -> Constraint
```

So we can, for example, apply `Plugin` to `MonadState Bool`, to
construct the `Type` of a plugin that can read and write the program
state:

```haskell
Plugin (MonadState Bool) :: Type
```

Defining plugins this way has satisfied the **capabilities** and
**abstract** requirements.  The capabilities (✓) are visible in the
type as abstract type class (✓) constraints.

In a real world scenario, there might be multiple parts of the main
program to hook into.  It is also useful to give a plugin a name, so
that the program can express which plugins are in use, report
errors, and so on.  So I turned `Plugin` into a data type with a
name field:

```haskell
data Plugin ctx = Plugin
  { pluginName :: String
  , pluginHook :: forall m. (ctx m) => Int -> m Int
  }
```

For applications with many hooks, it would be nice to wrap the hook
fields in `Maybe` (so the plugin doesn't have to implement them
all).  Alternatively, I could provide a helper function that
initialises the hooks to "no-op" functions.  Plugins would override
the hooks they use.  For this experiment there is only one hook, so
I've skipped this for now.


### Capabilities

The plugin system could be used by non-programmers; the types have
to make sense to them.  What is a `Plugin Applicative`?   What is a
`Plugin (MonadState Bool)`?!  I defined type synonyms for the
various constraints, to (hopefully) make the plugin system more
comprehensible to humans—programmer and non-programmer alike:

```haskell
type Pure = Applicative
type CanIO = MonadIO
type CanRWState = MonadState Bool
```

Now we can say `Plugin Pure`, or `Plugin CanRWState`.  This
addresses the **human-friendly** requirement (in part, at least).

What if a plugin needs to use multiple capabilities?  I first
approached this by defining a type synonym:

```haskell
type Unconstrained m = (CanIO m, CanRWState m)
```

Observe that `Unconstrained` has the required kind:

```haskell
λ> :k Unconstrained
Unconstrained :: (Type -> Type) -> Constraint
```

Unfortunately, we cannot use `Unconstrained` as an argument to
`Plugin`, because type synonyms cannot be partially applied:

```
λ> :k Plugin Unconstrained

<interactive>:1:1: error:
    The type synonym ‘Unconstrained’ should have 1 argument,
    but has been given none
```

The [*Unsaturated Type Families*][UTF] proposal, when it lands, will
hopefully lift this restriction.  Until then, the same effect can be
achieved with a type class and corresponding instance.  The
`FlexibleContexts`, `FlexibleInstances` and `UndecidableInstances`
extensions are required for these declarations:

[UTF]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0242-unsaturated-type-families.rst

```haskell
class (CanRWState m, CanIO m) => Unconstrained m where
  -- empty

instance (CanRWState m, CanIO m) => Unconstrained m where
  -- empty
```

These definitions give an instance for `Unconstrained` to any type
that already has instances for all the constituent capabilities.  It
was not necessary to mention `Pure`/`Applicative` because it is a
superclass of the other constraints.

### Implementing plugins

Now that I have defined the plugin type and capabilities, let's
implement some plugins.

#### `Plugin.Double`

`Plugin/Double.hs` defines a "pure" plugin that doubles the `Int`
value:

```haskell
module Plugin.Double where

import Plugin

plugin :: Plugin Pure
plugin = Plugin "Double" $ pure . (*2)
```

#### `Plugin.Offset`

`Plugin/Offset.hs` defines a "pure" plugin that adds a fixed offset
to the `Int` value.  The offset is configurable: applying `mkPlugin`
to the configuration yields the plugin value.  This satisfies our
**configuration** requirement.

```haskell
module Plugin.Offset where

import Plugin

mkPlugin :: Int -> Plugin Pure
mkPlugin offset = Plugin "Offset" $ pure . (+ offset)
```


#### `Plugin.FlipNegate`

`Plugin/FlipNegate.hs` defines a plugin that inverts the value of
`doNegate` in the program state.  Therefore it is a `Plugin
CanRWState`.

```haskell
module Plugin.FlipNegate where

import Control.Monad.State (modify)
import Plugin

plugin :: Plugin CanRWState
plugin = Plugin "FlipNegate" $ \i -> i <$ modify not
```


#### `Plugin.ShootLasers`

`Plugin/ShootLasers.hs` uses `CanIO` to shoot the lasers.  Watch
out!

```haskell
module Plugin.ShootLasers where

import Control.Monad.IO.Class (liftIO)
import Plugin

plugin :: Plugin CanIO
plugin = Plugin "ShootLasers" $
  \i -> i <$ liftIO (putStrLn "pew! pew!")
```


### Composition

The main program will have a list (or some other container) of zero
or more plugins.  And it will execute the plugin hooks at the
relevant part of the program.  OK, let's build a list of plugins in
`Main.hs`:

```haskell
plugins =
  [ Plugin.Double.plugin
  , Plugin.Offset.mkPlugin 100
  ]

main :: IO ()
main = …
```

So far so good!  Now let's add `ShootLasers` to the mix:

```haskell
plugins =
  [ Plugin.Double.plugin
  , Plugin.Offset.mkPlugin 100
  , Plugin.ShootLasers.plugin
  ]

main :: IO ()
main = …
```

Uh oh:

```
Main.hs:16:5: error:
    • Couldn't match type ‘MonadIO’ with ‘Applicative’
      Expected type: Plugin Pure
        Actual type: Plugin CanIO
    • In the expression: Plugin.ShootLasers.plugin
      In the expression:
        [Plugin.Noop.plugin, (Plugin.Double.plugin :: Plugin Pure),
         (Plugin.Offset.mkPlugin 100), Plugin.ShootLasers.plugin]
      In an equation for ‘plugins’:
          plugins
            = [Plugin.Noop.plugin, (Plugin.Double.plugin :: Plugin Pure),
               (Plugin.Offset.mkPlugin 100), ....]
   |
16 |   , Plugin.ShootLasers.plugin
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^
```

`Applicative` is a superclass of `MonadIO`.  Abstract types with
these constraints can unify, but the concrete types `Plugin MonadIO`
and `Plugin Applicative` do not unify.  We cannot put them together
in a list.

So when composing plugins, we need a way to *relax* the constraints
of individual plugins to the "lowest common denominator".  That is,
we have to construct a list of `Plugin Unconstrained`.  Therefore we
need a function to turn a `Plugin ctx` into a `Plugin
Unconstrained`, provided that `Unconstrained` "encompasses" `ctx`.
The `relax` function accomplishes this:

```haskell
relax
  :: (forall m. ctx' m => ctx m)
  => Plugin ctx -> Plugin ctx'
relax (Plugin n fs) = Plugin n fs
```

Note the constraint: `(forall m. ctx' m => ctx m)`.  This requires
the `QuantifiedConstraints` language extension.  It means that the
function is defined only if `ctx` (the *input* plugin capability) is
implied by `ctx'` (the *output* capability).

Now we can use `relax` to construct a `[Plugin Unconstrained]`,
because `Unconstrained` implies all of the individual capabilities
available to plugins:

```haskell
plugins :: [Plugin Unconstrained]
plugins =
  [ relax Plugin.Double.plugin
  , relax (Plugin.Offset.mkPlugin 100)
  , relax Plugin.FlipNegate.plugin
  , relax Plugin.ShootLasers.plugin
  ]

main :: IO ()
main = …
```

The prototype now satisfies the **composition** requirement.


### Executing plugins

The final step is to update the main program to execute the plugins.
Where previously we had `pure i` (and a `TODO` comment), we now have
the Kleisli composition (monadic chaining via `(>=>)`) of the plugin
hook functions, applied to `i`:

```haskell
main :: IO ()
main = do
  -- read start values
  doNegate <- prompt "negate number? [True|False]"
  i <- prompt "number (Int)"

  (j, doNegate') <- flip runStateT doNegate $
    foldr (>=>) pure (fmap pluginHook plugins) i

  -- print result
  let r = if doNegate' then negate j else j
  putStr "result: " *> print r

  where
  prompt s = putStr (s <> ": ") *> hFlush stdout *> readLn
```

This time running the program, telling it to negate the input and
giving it the value `10`, gives the following transcript:

```
ftweedal% ./Main
negate number? [True|False]: True
number (Int): 10
pew! pew!
result: 120
```

We can see that:

- `Double` doubled the input to `20`
- `Offset` added the configured offset of 100, giving `120`
- `FlipNegate` turned negation off
- `ShootLasers` shot the lasers (hopefully they were not pointed at
  your foot)

As a result, the final output of the program was `120`.  Note that
the order of plugins is significant.  Plugins appearing earlier in
the list are executed earlier.


## Discussion

### Paranoia

It's all good and well that plugins express their types.  But what's
to stop a `Plugin Pure` sneakily evolving into a `Plugin CanIO` upon
a new release, and compromising your system?

The paranoid user can mitigate this risk by providing explicit type
signatures for each plugin installed in the main program:

```haskell
plugins :: [Plugin Unconstrained]
plugins =
  [ relax (Plugin.Double.plugin :: Plugin Pure)
  , relax (Plugin.Offset.mkPlugin 100 :: Plugin Pure)
  , relax (Plugin.FlipNegate.plugin :: Plugin CanRWState)
  , relax (Plugin.ShootLasers.plugin :: Plugin CanIO)
  ]
```

If any plugin releases a new version that require additional (or
fewer) capabilities its type will change, resulting in a type error.
For example, if `Double` becomes a `Plugin CanIO`, GHC gives the
following type error:

```
Main.hs:14:12: error:
    • Couldn't match type ‘MonadIO’ with ‘Applicative’
      Expected type: Plugin Pure
        Actual type: Plugin CanIO
    • In the first argument of ‘relax’, namely
        ‘(Plugin.Double.plugin :: Plugin Pure)’
      In the expression: relax (Plugin.Double.plugin :: Plugin Pure)
      In the expression:
        [relax (Plugin.Noop.plugin :: Plugin Pure),
         relax (Plugin.Double.plugin :: Plugin Pure),
         relax (Plugin.Offset.mkPlugin 100 :: Plugin Pure),
         relax (Plugin.FlipNegate.plugin :: Plugin CanRWState), ....]
   |
14 |   , relax (Plugin.Double.plugin :: Plugin Pure)
   |            ^^^^^^^^^^^^^^^^^^^^
```

Thanks to the constraint type synonyms, the type error is (in my
opinion) understandable.  Perhaps even non-programmers could make
sense of it.  Unfortunately, it doesn't hint at what a user should
do to solve it.  A better error message would suggest to use `relax`
or to review the plugin's type.  GHC has some support for
[user-defined type errors][], but at this time it is not possible to
augment ordinary type mismatch errors.

[user-defined type errors]: https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-TypeLits.html#g:4


### Multiple hook functions

In real world applications, there can be multiple kinds of plugin
functions relating to different behaviours in the main program.  For
example, Purebred will have a hook for processing a message prior to
displaying it, and another hook for manipulating outgoing emails.
Some plugins will only use a single hook, but others will use
multiple hooks.  For example, an OpenPGP plugin would perform
decryption and signature verification when preparing a message for
display, and perform signing and/or encryption when sending mail.

It is possible that a plugin might require different capabilities
for different hooks. This poses a complex design question, with
several possible solutions:

- Should all hooks have their design constraints expressed
  *separately* at the type level?  For example, a plugin type with
  two hook functions could be written as `Plugin ctx1 ctx2`.  The
  main problem with this is that adding new hooks to the plugin type
  is an API-breaking change.  It also isn't particularly
  human-friendly because the type doesn't reveal which operation
  each constraint applies to.

- Should different hooks have different types?  Instead of a unified
  `Plugin` type, for Purebred we could have a `SendHook` and
  `DisplayHook`.  The downside is that installing a plugin that uses
  multiple hooks means installing all those hooks separately.  If
  the plugin evolves to use more (or fewer) hooks, the user has to
  do extra work to migrate.

- Should plugins be a single data type, with a single constraint?
  The downside to this approach is the type does not express which
  hooks are used, or what the per-hook required capabilities are.
  The plugin's single capability constraint is the union of all
  capabilities required by all hook functions.

I chose the last approach, for a few reasons.  First, when it comes
to I/O in particular, if you are trusting *any* part of the plugin
with `CanIO`, from a risk perspective it doesn't matter much that
*other* parts cannot perform I/O.  Second, I suspect that plugins
that use multiple hooks *and* use different capabilities for those
hooks will be uncommon.  For me, prioritising API stability and ease
of use made the choice easy.  But it is still worthwhile to discuss
the tradeoffs and alternatives.


### Implementation considerations

Some implementation considerations that arise when applying this
plugin system design in Purebred (real-world programs in general)
include:

- Plugins have to be be stored in the internal configuration object.
  Should the hook functions be decomposed early (storing multiple
  lists of hook functions in the config) or just-in-time (storing a
  single list of plugins, and extracting the relevant hook functions
  when they are needed)?  I will initially store a single list of
  plugins.

- It would be nice to provide information about which plugins are
  installed alongside version info (`--version`), "about" UI, etc.

- Some standard functionality could be implemented using the plugin
  system.  So, should it be?  And should "wired-in" plugins be
  advertised or hidden?

- What interface(s) should be given for plugin installation?  To
  what extent should plugins be isolated from each other?

- Some plugins will not use all hooks.  Purebred might end up having
  many hooks, but most plugins will use only one or a few of them.
  To make it convenient to define plugins, I can define a helper
  function to construct a new plugin whose hooks are all no-ops.
  Plugin implementers can override the hooks they need.

- Some plugins might need to modify the UI and/or propagate data
  around.  For example, an OpenPGP plugin should let users choose
  whether to encrypt, sign, or do nothing with an outbound email.  I
  haven't solved this problem yet, but I'll need to, somehow.


## Conclusion

My goal was to design a plugin system for Purebred offering
**composition**; **capabilities** expressed through
**human-friendly** types, enforced by the type system; and
**abstract** plugin implementations so that the main program can
evolve.  Additionally, there had to be a way to provide
**configuration** to plugins that require it.  The prototype plugin
system described in this post satisfies these requirements.

I left several regions of the plugin system design space unexplored,
including dynamic (re)loading of plugins and plugin-specific data
and state.  These are interesting and challenging problems, but they
are not problems that the Purebred project needs to solve right now.

The next step is to take what I've learned from this prototype and
implement it in Purebred.  There will be some new challenges there,
and I expect the experience to provide ample material for a
follow-up blog post—or several.
