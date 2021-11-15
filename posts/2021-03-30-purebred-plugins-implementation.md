---
tags: plugins
---

# Purebred plugin system: implementation

I previously wrote about a [prototype plugin system][prototype-post]
for [Purebred][].  In this post I discuss some improvements to the
design, and report on the implementation progress.

[prototype-post]: 2021-02-02-plugin-system-prototype.html
[Purebred]: https://github.com/purebred-mua/purebred

## Recap

The requirements for the plugin system were:

- Plugin types must express the **capabilities** they use, to help
  users understand what the plugin can and cannot do.

- Diverse plugins must **compose** together, even when they use
  different capabilities.

- Plugin function types must use **abstract** constraints, not
  concrete types, so the application can evolve without breaking
  plugins.

- Types should be **human friendly**.

- It must be possible to **configure** plugins, if required.

The prototype design satisfied these requirements, with some
caveats.  The biggest deficiencies were that the plugin's type does
not reveal which hook(s) the plugin uses, and the plugin's type
expresses the *union* of capabilities needed by all its hook
functions.  There was no way for a plugin to say, for example, that
it uses `CanIO` for one hook, and `CanRWState` for some other hook.


## Plugin record type improvements

Recall the `Plugin` record type from the previous article:

```haskell
data Plugin ctx = Plugin
  { pluginName :: String
  , pluginHook :: forall m. (ctx m) => Int -> m Int
  }
```

`ctx` is the capability, for example `CanIO`.  The many hooks in a
real application would appear as additional fields in this record
type.  One consequence of this design is that the capability of the
plugin as a whole must be the union of capabilities actually
required by the plugin's hook functions.

It also means that there is no way for the application to offer
limited capabliities to some hooks.  Put another way, all
capabilities are available to all hooks.  This does not reflect the
needs of real applications; they may need to restrict which
capabilities are available in different hooks.

The plugin system implemented in Purebred improves on the prototype
design.  We define the plugin record type, now called `PluginDict`,
as follows:

```haskell
data PluginDict = PluginDict
  { _pluginName :: String
  , _pluginVersion :: Version
  , _pluginBuiltIn :: Bool
  , _configHook :: ConfigHook CanIO
  , _preSendHook :: PreSendHook Unconstrained
  }
```

The `ctx` type parameter is gone.  Instead, each hook function field
specifies the capabilities available to that hook.

Each hook function is no longer a bare function but is wrapped in a
`newtype`.  This (I feel) improves readability.  It allows lenses to
be defined, without resorting the GHC's currently-flaky support for
impredicative types.  Use of optics is also why the field accessors,
which are not exported, are prefixed with `_`.

As an example of a hook type, here is the definition of
`PreSendHook`:

```haskell
newtype PreSendHook cap = PreSendHook
  ( forall m. (cap m) => MIMEMessage -> m MIMEMessage )

getPreSendHook
  :: (cap m)
  => PreSendHook cap -> MIMEMessage -> m MIMEMessage
getPreSendHook (PreSendHook f) = f
```

::: {.note #note-field-accessors}

`getSendPreHook` must be a standalone function, not a field
accessor.  This is because **GHC 9.0.1** and later preserve the
written order of quantified type variables in field selectors
([release note][]):

[release note]: https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/9.0.1-notes.html#language

```haskell
λ> data T c = T { unT :: forall m. (c m) => () -> m () }
λ> :t unT
-- GHC 8.10.5
unT :: c m => T c -> () -> m ()
-- GHC 9.0.1
unT :: T c -> forall (m :: * -> *). c m => () -> m ()
```

:::

Finally, I added a field to store the plugin version, and a boolean
to distinguish between built-in and external plugins.  We (the
Purebred authors) intend to use the plugin system to provide some
baseline functionality.  But we do not want to treat these
behaviours as plugins from the user's point of view.  The
`_pluginBuiltIn` field lets us discriminate.

## Public plugin type improvements

Plugin modules no longer export a plugin record value (what we now
call `PluginDict`).  The `Plugin` type lives on, in a different
form:

```haskell
data Plugin hooks = Plugin String Version hooks
```

A `Plugin` value has a name, version, and `hooks`.  What is `hooks`?
It is best understood in the context of the `usePlugin` function.
Like the `relax` function from the prototype, `usePlugin`
monomorphises plugins and prepares them for use in the main program.

```haskell
usePlugin :: (Hook hooks) => Plugin hooks -> PluginDict
usePlugin (Plugin name ver hook) =
  setHook hook $
    PluginDict name ver False
      (ConfigHook pure)
      (PreSendHook pure)
```

`usePlugin` constructs a `PluginDict` full of *no-op* hooks, then
sets the plugin's hooks via the `setHook` function.  `hooks` must
have an instance of the `Hook` type class:

```haskell
class Hook t where
  setHook :: t -> PluginDict -> PluginDict
```

Each hook function type has an instance of `Hook`.  These instances
set the hook function in the `PluginDict`.  A quantified constraint
ensures the capabilities demanded by the hook function do not exceed
the capabilities offered.  Here, as an example, is the instance for
`PreSendHook`:

```haskell
instance
   (forall m. Unconstrained m => cap m)
    => Hook (PreSendHook cap) where
  setHook (PreSendHook f) = set preSendHook (PreSendHook f)
```

This is another reason why it was necessary to `newtype` all the
hook functions.

To support plugins that use multiple hooks, we declare an instance
of `Hook` for pairs (2-tuples).  This allows plugins to define as
many hooks as they need, using nested tuples:

```haskell
instance (Hook h1, Hook h2) => Hook (h1, h2) where
  setHook (h1, h2) = setHook h1 . setHook h2
```

## Implementing plugins

Plugins construct and export a `Plugin hook` value, where `hook` is
a hook function type or a nested tuple of the same.

::: note

If a plugin defines the same hook multiple times, `usePlugin`
discards all but the "leftmost" occurrence.

:::

Here is the implementation of our `User-Agent` plugin:

```haskell
module Purebred.Plugin.UserAgent (plugin) where

import Control.Lens (set, view)
import Data.MIME (headerText)
import Purebred.Plugin
import Purebred.Version (version, userAgent)
import Purebred.Types (confCharset)

plugin :: Plugin (PreSendHook CanReadConfig)
plugin = Plugin "UserAgent" version (PreSendHook hook) where
  hook msg = do
    charsets <- view confCharsets
    let l = headerText charsets "User-Agent"
    pure $ set l (Just userAgent) msg
```

The concrete type of a plugin shows the hook(s) used by the plugin,
and the capabilities required by each hook.  If a plugin needs to
use multiple hooks, use nested pairs.  In the example above,
`plugin` uses the `PreSendHook` with the `CanReadConfig` capability.
The type proves that the the `User-Agent` plugin only uses the
`PreSendHook`, cannot perform I/O, and so on.

::: note

We could define `Hook` instances for 3-tuples, 4-tuples, and so on.
But I decided not to, because where does it end?  Besides, I don't
think there will be many plugins that use more than two hooks.

:::

The name of the plugin value (`plugin` in the preceding example) is
not important.  Plugin authors can use whatever name makes sense.
Plugins that require configuration should export a function instead
of a plain `Plugin` value, as in the following examples:

```haskell
module Purebred.Plugin.TweakConfig where

import Purebred.Plugin
import Purebred.Version (version)
import Purebred.Types (UserConfiguration)

tweakConfig
  :: (UserConfiguration -> UserConfiguration)
  -> Plugin (ConfigHook Pure)
tweakConfig hook =
  Plugin "Purebred.Plugin.TweakConfig" version
    (ConfigHook (pure . hook))

tweakConfigWithIO
  :: ( forall m. (CanIO m) =>
       UserConfiguration -> m UserConfiguration )
  -> Plugin (ConfigHook CanIO)
tweakConfigWithIO hook =
  Plugin "Purebred.Plugin.TweakConfig (IO)" version
    (ConfigHook hook)
```

The `TweakConfig` module provides two variants of a plugin to adjust
Purebred's configuration at startup.  `tweakConfig` takes a pure
transformation and yields a `Plugin (ConfigHook Pure)`, whereas
`tweakConfigWithIO` allows the use of `IO`.

## Using plugins

Users apply `usePlugin` to each plugin and produce a `[PluginDict]`.
That list is then given as argument to the main Purebred entry
point:

```haskell
purebred :: [PluginDict] -> IO ()
```

The user configuration file, in a basic sense, is a program that
constructs a list of plugins and applies `purebred` to it.  Here is
a cut-down version of my `~/.config/purebred/purebred.hs`:

```haskell
import Purebred
import qualified Purebred.Plugin.TweakConfig
import qualified Purebred.Plugin.ICU

listKeybindings =
  [ {- my preferred keybindings -} ]

tweak =
    over (confIndexView . ivBrowseThreadsKeybindings)
         (listKeybindings <>)
  . set (confNotmuch . nmNewTag) "inbox"

main = purebred
  [ usePlugin $
      Purebred.Plugin.TweakConfig.tweakConfig tweak
  , usePlugin
      Purebred.Plugin.ICU.plugin
  ]
```

Some plugins have no configuration and just do their thing.  But
other plugins may require the user to construct a substantial
configuration.  A PGP/MIME plugin that uses GnuPG, though not
written yet, seems likely to have a lot of knobs.  Plugins that
require configuration should offer ergonomic ways to construct
sensible, safe configurations.

### Built-in plugins

`defaultConfig` is the default `UserConfiguration` value.  It is not
exported, but it can be modified by `ConfigHook`s.  Built-in plugins
are already set in the `confPlugins` field of `defaultConfig`:

```haskell
defaultConfig :: UserConfiguration
defaultConfig = Configuration
  { _confPlugins = set pluginBuiltIn True <$>
      [ usePlugin Purebred.Plugin.UserAgent.plugin
      …
      ]
  …
  }
```

`usePlugin` sets the `pluginBuiltIn` field to `False`.  But we reset
it to `True` for all built-in plugins.  The `pluginBuiltIn` optic is
not exported.  Therefore users cannot change the treatment of a
plugin from built-in to external, or vice versa.

The `purebred` entry point merges user-supplied plugins with the
built-in plugins:

```haskell
purebred :: [PluginDict] -> IO ()
purebred plugins = do
  …
  let
    cfg = over confPlugins (plugins <>) defaultConfig
    dyreParams = …
  Dyre.wrapMain dyreParams cfg
```


## Executing plugins

Purebred executes `ConfigHook`s immediately after Dyre (the
configuration system) invokes the "real main" action (called
`launch`):

```haskell
launch :: UserConfiguration -> IO ()
launch inCfg = do
  let
    plugins = view confPlugins inCfg
    hooks = getConfigHook . view configHook <$> plugins
  cfg <- foldr (>=>) pure hooks inCfg
  …
```

`PreSendHook`s are executed in the action that sends mail:

```haskell
  let msg = …
  hooks <- uses (asConfig . confPlugins)
           (fmap (getPreSendHook . view preSendHook))
  cfg <- use asConfig
  msg' <- runReaderT (foldr (>=>) pure hooks msg) cfg
  k (buildMessage msg')
```

These two examples reveal a pattern for hook execution:

1. Extract the relevant hook functions from the `[PluginDict]`

2. Use Kleisli composition `(>=>)` to fold the list into a single
   action.

3. Execute the composed action, using transformers if necessary.

This pattern applies when the hook function type has the shape `a ->
m a`.  So far, all the hook functions have that shape.


## Hooks in Purebred

The hooks we have already implemented are:

- **`ConfigHook`**: modify configuration at program startup.

- **`PreSendHook`**: modify or process a message immediately prior
  to sending.  We currently use this hook, in a built-in plugin, to
  add a `User-Agent` header to outgoing messages.  Can perform I/O.
  This is the hook that will be used to sign and/or encrypt outgoing
  mail.  We will probably also add a capability to enable a plugin
  to abort sending.

Hooks we haven't implemented, but must, include (names subject to
change):

- **`DisplayHook`**: modify or process a message before displaying
  it.  One use case is to perform decryption or verify signatures.

- **`PreEditHook`**: process a part before editing it.  Together
  with `PostEditHook` this could be used to enable editing of
  headers along with text bodies.

- **`PostEditHook`**: process a message part after editing it.

We have thought of some other hooks that seem useful, but haven't
yet committed to implementing:

- **`CreateHook`**: modify a message immediately after creation
  (i.e. before editing).  A proposed use is appending "signature"
  content.

- **`ReadHook`**: process raw message data when reading from disk.
  A plugin could use this to detect compressed files and inflate
  them.  Another use case could be to attempt to "repair" corrupt or
  nonconformant messages.

Purebred also needs an "address book" interface.  We want plugins to
be able to provide address book behaviour.  But we haven't designed
it yet.  It remains to be seen whether we will do it by way of hooks
(as described in this post), or by updating the main configuration
(via `ConfigHook`), or by some other means.


## Discussion

Although I felt that the prototype design did satisfy the
*capabilities* requirement, there were some deficiencies.  I
identified and discussed these in [the original
article][prototype-post].  In particular, the `Plugin` type did not
express which hooks the plugin uses, nor could a plugin acquire
different capabilities for different hooks.  The updated design
eliminates these deficiencies.

Plugins now have a version field, and the internal representation
also distinguishes between built-in and external plugins.  We use
this to hide built-ins when listing plugins in the `--version`
output.

The problems of UI interaction, and how plugins can store and use
plugin-specific state, remain unsolved.

There is also the question of hook execution order or priority.
Hook functions process and potentially modify a datum of interest,
such as a `Message` or a `ByteString`.  Consider `ReadHook`, for
processing messages as they're read from disk, and its hypothetical
counterpart `WriteHook`.  A plugin for on-disk mail compression would use
these hooks.  When reading mail, decompression must precede other
operations.  When writing, compression should be the final step.
The current implementation runs plugins hooks in the order they're
stored—external plugins first, then built-ins.  So plugins that
involve dual operations (compression, encryption) present inherent
challenges.  And there is no way to tame unwanted interactions
between external and built-in plugins.

For now, the ordering problem is theoretical.  I haven't encountered
it in practice, because few plugins have been implemented.  So I'm
not going to try to solve the problem prematurely.  I have an idea
that does not add much complexity and should be simple for users to
understand.  But I will save that discussion for a future post.
