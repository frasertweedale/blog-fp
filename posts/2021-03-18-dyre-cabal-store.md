---
tags: dyre, packages, ghc, cabal
---

# How Dyre works with Cabal Nix-style builds

[Dyre][dyre-hackage] is a tool for Haskell program configuration by
(re)compilation.  In [my previous post][] I discussed the changes
planned for the Dyre 0.9 release.  That release happened a couple of
days ago… but I soon discovered a critical bug in Dyre's support for
*cabal-install* Nix-style local builds.

I have now [fixed the bug][] and [released v0.9.1][].  As a result
of the experience I decided to write this post about how Nix-style
local builds work, and how Dyre works with programs built and
installed that way.

[dyre-hackage]: https://hackage.haskell.org/package/dyre
[my previous post]: 2021-02-21-dyre-0.9-rc.html
[fixed the bug]: https://github.com/willdonnelly/dyre/commit/414d961222b5b864933f35f0a0d74b1a2f501a23
[released v0.9.1]: https://hackage.haskell.org/package/dyre-0.9.1

## GHC and package databases

Haskell packages are installed in *package databases*.  The package
database contains libraries' object code and Haskell interface
(`*.hi`) files, as well as package descriptions and references to
the library's dependencies.  When compiling and linking a program or
library, GHC looks for dependencies in the configured package
database(s).

By default, GHC knows about two package databases.  The *global
package database* is where *base* and the other libraries bundled
with GHC are installed.  The database location is tied to the GHC
release; for example: `/usr/lib64/ghc-8.8.4/package.conf.d`.  On
most operating systems, the system package manager will install
Haskell libraries in the global package database.

GHC also knows about *user package databases*.  On Unix systems
these are located at `~/.ghc/ARCH-OS-GHCVER/package.conf.d`.  As the
name and location suggests, unprivileged users can install packages
to their user package database.  GHC will also search for
dependencies in there, unless told otherwise.

*cabal-install* (the `cabal` program) and the system package manager take care of these
package databases.  Users don't normally need to look into or modify
them.  For the curious (or cursed), you can use the `ghc-pkg`
program to inspect and modify package databases.  For example,
here's how to list the contents of the global package database:

```shell
% ghc-pkg --global list
/usr/lib64/ghc-8.8.4/package.conf.d
    Cabal-3.0.1.0
    Glob-0.10.0
    HTTP-4000.3.14
    HsYAML-0.2.1.0
    HsYAML-aeson-0.2.0.0
    … (many more)
```

Let's have a look at one of the records in the global package
database:

```
% cat /usr/lib64/ghc-8.8.4/package.conf.d/mtl-2.2.2.conf
name:                 mtl
version:              2.2.2
visibility:           public
id:                   mtl-2.2.2
key:                  mtl-2.2.2
license:              BSD-3-Clause
maintainer:           Edward Kmett <ekmett@gmail.com>
author:               Andy Gill
homepage:             http://github.com/haskell/mtl
synopsis:             Monad classes, using functional dependencies
description:
    … (elided)
category:             Control
abi:                  7208c11cc6615ddff9d903a90a9878f8
exposed:              True
exposed-modules:
    … (elided)
import-dirs:          /usr/lib64/ghc-8.8.4/mtl-2.2.2
library-dirs:         /usr/lib64/ghc-8.8.4/mtl-2.2.2
dynamic-library-dirs: /usr/lib64
data-dir:             /usr/share/x86_64-linux-ghc-8.8.4/mtl-2.2.2
hs-libraries:         HSmtl-2.2.2
depends:              base-4.13.0.0 transformers-0.5.6.2
haddock-interfaces:   /usr/share/doc/ghc/html/libraries/mtl-2.2.2/mtl.haddock
haddock-html:         /usr/share/doc/ghc/html/libraries/mtl-2.2.2
```

Most fields are self explanatory, but some need a little
explanation:

- **`id`** is the *unit ID* that uniquely identifies the package.  In
  the example above it includes the package `name` and `version`;
  a unit ID can also include other identifying data.

- **`depends`** lists the direct dependencies of this package, as
  unit IDs.

- **`exposed`** controls whether the modules in the package are
  available to imported during compilation.  This is just the
  default.  GHC command line options and [*package environment
  files*][package-environments] can alter package visibility.

For more details see the [*Packages* chapter][ghc-guide-packages] of
the GHC User's Guide.

[package-environments]: https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/packages.html#package-environments
[ghc-guide-packages]: https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/packages.html

## *cabal-install* and package databases

In earlier times, the *cabal-install* build tool would install all
dependencies of a package into the global or user package database.
But when you have multiple packages with conflicting dependencies,
this approach does not work.  It also makes updating packages very
risky.  Updating one package (because some dependent program or
library requires a newer version) would often break other installed
dependent packages.

*cabal-install*'s first solution to this problem was [per-project
sandboxes][].  When using a sandbox, *cabal-install* installs
dependencies (those that are not in the global or user package
database) under `.cabal-sandbox/` in the project directory.
Sandboxes are isolated from each other.  This solved the main
problem but introduced another.  Many projects meant many sandboxes,
wasting a lot of time and space as common dependencies got built
over and over again, in different sandboxes.

Furthermore, users had to deliberately create sandboxes.  The
problematic behaviour (install to the user package database) was the
default behaviour.  You had to *know* about, and use, sandboxes to
avoid trouble.

::: note
Sandbox support was removed in *cabal-install* 3.4.
:::

[per-project sandboxes]: https://cabal.readthedocs.io/en/3.2/installing-packages.html#developing-with-sandboxes


## Nix-style local builds

*cabal-install* since version 1.24 supports [*Nix-style local
builds*][v2-build].  It is the default behaviour (i.e. what `cabal
build` does) since version 2.0.  The feature is also called
*`v2-build`* or *`new-build`*

[v2-build]: https://cabal.readthedocs.io/en/3.4/nix-local-build-overview.html

This build system caches dependencies in a package database at
`~/.cabal/store/GHCVER/package.db`.  I call this package database
the ***Cabal store***.  The unit IDs of packages in the Cabal store
include the name, version, and a digest (hash) of the entire package
contents, which prevents collisions.  For example, `ghc-pkg` can
print the unit IDs of all the variants of *purebred-email* in my
Cabal store.  Note that there are multiple variants even of
particular versions (some were development builds):

```shell
% ghc-pkg \
    --package-db ~/.cabal/store/ghc-8.8.4/package.db \
    field purebred-email id \
    | sort
id: purebred-email-0.4.2-904f4dca7fc38b0732875f3118229289f0a854a1b29ceb6f61952455fc536475
id: purebred-email-0.4.2-937f77fe675ce899d5b8bf4e451f09d83f458db59751e28d393dafa5d0e46e80
id: purebred-email-0.4.3-12899342c54e9be6f8bfabf40ea03e13ba08a15da3d519a6ba6c16b5e8a2fdf7
id: purebred-email-0.4.3-132e0f4a654056feced84d90a98b5cc3f8635b2a56e2821e5193cace1a73946d
id: purebred-email-0.5.0-04c8aceff5a68a6eb0537475f958dcb2aa763675c36a57893119534e690ebfe7
id: purebred-email-0.5.0-0a5c9f98d485c79e5bbbb3eefbc8239c4e5a74dc26d03fc0a12ce8a45a304c8d
id: purebred-email-0.5.0-4c9fa1b81ac8638346df176519fee11541953f243502699a8693f69fafc2293f
id: purebred-email-0.5.0-cc781fa90dc462076d5be322d38e59b76a1604cb866e8b6a15a150653e9f6595
id: purebred-email-0.5.0-e8d411ef4593a88bbd40293b523c03079a01138f9bb1278781a4eb5a15cf461f
```

If we inspect the `depends` fields of one of these packages, we see
a mix of unit IDs with and without hashes.  The ones without hashes
will be located in the global package database, and the ones with
hashes are other packges in the Cabal store:

```shell
% ghc-pkg \
    --package-db ~/.cabal/store/ghc-8.8.4/package.db \
    --unit-id \
    field purebred-email-0.4.3-12899342c54e9be6f8bfabf40ea03e13ba08a15da3d519a6ba6c16b5e8a2fdf7 \
    depends
depends: attoparsec-0.13.2.4-6pdJvsCYDtQ1ZikLcQPH6i base-4.13.0.0
         base64-bytestring-1.0.0.3-6nNbupf0oNc9ekFOSwyfJw
         bytestring-0.10.10.1 case-insensitive-1.2.1.0-GK3nA3zBZszGkFybfKiPH
         concise-0.1.0.1-9e9a3f5d1a0cb55cc27d6ed4559347484ed0182db25d17f54fecc4cb16c664f3
         deepseq-1.4.4.0 lens-4.18.1-1Ci7u4sFw8083dvBI6HqCw
         semigroupoids-5.3.4-3QeAlkxMhKa1rjJSsIVWj7
         semigroups-0.19.1-LKdktuRRdRZ1yx9gBXzSbK
         stringsearch-0.3.6.6-ea9567d6b6c3d6f0b0229eaa65b28da6602e2135e50018657a4d1545dc4a0d0c
         text-1.2.4.0 time-1.9.3
```

Object code filenames of libraries installed in the Cabal store
incorporate the full unit ID:

```shell
% cd ~/.cabal/store/ghc-8.8.4/purebred-email-0.4.3-12899342c54e9be6f8bfabf40ea03e13ba08a15da3d519a6ba6c16b5e8a2fdf7/lib
% ls
Data
libHSpurebred-email-0.4.3-12899342c54e9be6f8bfabf40ea03e13ba08a15da3d519a6ba6c16b5e8a2fdf7.a
libHSpurebred-email-0.4.3-12899342c54e9be6f8bfabf40ea03e13ba08a15da3d519a6ba6c16b5e8a2fdf7-ghc8.8.4.so
```

To assist the dynamic linker, the `RUNPATH` attribute in dynamically
linked executables and shared objects includes the subdirectories of
the Cabal store where the needed shared objects are located:

```shell
% objdump -x libHSpurebred-email-0.4.3-12899342c54e9be6f8bfabf40ea03e13ba08a15da3d519a6ba6c16b5e8a2fdf7-ghc8.8.4.so \
    | grep RUNPATH \
    | awk '{ print $2 }' \
    | tr : '\n' \
    | sort
/home/ftweedal/.cabal/store/ghc-8.8.4/concise-0.1.0.1-9e9a3f5d1a0cb55cc27d6ed4559347484ed0182db25d17f54fecc4cb16c664f3/lib
/home/ftweedal/.cabal/store/ghc-8.8.4/stringsearch-0.3.6.6-ea9567d6b6c3d6f0b0229eaa65b28da6602e2135e50018657a4d1545dc4a0d0c/lib
/usr/lib64
/usr/lib64/ghc-8.8.4/rts
```

Executables are installed in the Cabal store and referenced by a
symbolic link installed in `~/.cabal/bin/` or a similar location.

```shell
% which purebred
~/.cabal/bin/purebred
% readlink ~/.cabal/bin/purebred
../store/ghc-8.8.4/purebred-0.1.0.0-ffa8d363a9788cf11f71e6dbad787526a4fa1f9f750969f4d55036610f5cb027/bin/purebred
```

## Dyre and the Cabal store

When using *cabal-install*, the details about the Cabal store are
hidden from the user.  But a Dyre-enabled program does not have the
luxury of using *cabal-install* when (re)compiling a custom
executable.  We cannot assume that the `cabal` program is available.
Even if we did make that assumption, using *cabal-install* would
introduce **a lot** of additional complexity.  And maintenance
overhead, because *cabal-install*'s behaviour is continually
evolving, whereas GHC's command line interface is stable.

So Dyre invokes GHC directly.  When a Dyre application's main
library is in the global or user package database, GHC finds it and
all is well.  But when the library is in the Cabal store, GHC cannot
find it:

```shell
% purebred
Configuration '/home/fraser/.config/purebred/purebred.hs' changed. Recompiling.
Error occurred while loading configuration file.
purebred: 
/home/fraser/.config/purebred/purebred.hs:5:1: error:
    Could not find module ‘Purebred’
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
  |
5 | import Purebred
  | ^^^^^^^^^^^^^^^

CallStack (from HasCallStack):
  error, called at src/Purebred.hs:347:69 in purebred-0.1.0.0-95b0b0050ea0df258f6dce7f45c7c58630549c4395e1e143f3e0915c057e8cbc:Purebred
```

To overcome this, Dyre needs to detect when the application library
is installed in the Cabal store, and pass some additional command
line options to GHC:

- The `-package-db <FILE>` option adds the package database at
  `FILE` to the list of databases GHC searches.  Dyre uses this
  option to add the Cabal store.

- The `-package-id <UNIT-ID>` option *exposes* the package
  identified by `UNIT-ID`, making its public modules available for
  import.  Dyre uses this option to expose the main application
  library.

In Dyre's implementation, the `getCabalStoreGhcArgs` function
computes these options.  It is applied to a Dyre project name and a
library path.  If the library path is in the Cabal store *and*
matches the project name, Dyre derives the appropriate `-package-db`
and `-package-id` options.

```haskell
import System.FilePath
  (joinPath, splitPath, dropTrailingPathSeparator)

getCabalStoreGhcArgs :: String -> FilePath -> [String]
getCabalStoreGhcArgs proj =
    mkArgs . go . fmap dropTrailingPathSeparator . splitPath
  where
  go (".cabal" : "store" : hc : pid : _) =
    case splitOn '-' pid of
      [name, _version, _hash] | name == proj
        -> Just (pid, [".cabal", "store", hc, "package.db"])
      _ -> Nothing
  go (h : t@(_ : _ : _ : _ : _)) = fmap (h:) <$> go t
  go _ = Nothing

  mkArgs Nothing = []
  mkArgs (Just (unitId, pkgDb)) =
    ["-package-db", joinPath pkgDb, "-package-id", unitId]

  splitOn a l = case span (/= a) l of
    (h, []) -> [h]
    (h, _ : t) -> h : splitOn a t
```

::: note

*cabal-install* lets you [override the Cabal store directory].  Dyre
assumes the default `~/.cabal/store/`.  If the Cabal store is not in
the expected place, the detection logic fails.

:::

[override the Cabal store directory]: https://cabal.readthedocs.io/en/3.4/cabal-project.html?highlight=store-dir#cmdoption-store-dir

The final piece of the puzzle is how Dyre learns the application's
library directory that will be the `FilePath` argument to
`getCabalStoreGhcArgs`.  Dyre itself is just a library.  We rely on
the main program to tell Dyre where its library is located.  The
only alternative I know of is brittle, operating system-specific
hacks.

Cabal packages can make use of an auto-generated [`Paths_<pkgname>`
module][Paths_].  It provides `IO` actions that return the
installation paths of the package (object files, binaries, data and
so on), including:

```haskell
getLibDir :: IO FilePath
```

[Paths_]: https://cabal.readthedocs.io/en/3.4/cabal-package.html#accessing-data-files-from-package-code

Dyre applications can read this value and pass it to Dyre via the
[`includeDirs`][includeDirs] field in the `Params` object.  Here is
a simplified example:

[includeDirs]: https://hackage.haskell.org/package/dyre-0.9.1/docs/Config-Dyre.html#v:includeDirs

```haskell
module MyApp where

import qualified Config.Dyre as Dyre
import Paths_myapp (getLibDir)

data Configuration = …

defaultConfig :: Configuration
defaultConfig = …

realMain :: Configuration -> IO ()
realMain = …

main :: IO ()
main = do
  libdir <- getLibDir
  let params =
        ( Dyre.newParams "myapp" realMain (const error) )
        { Dyre.includeDirs = [libdir] }
  Dyre.wrapMain params defaultConfig
```

::: note

For Cabal store detection to work, the Dyre project name (`myapp` in
the example above), **must** be the same as the library package
name.  Otherwise `getCabalStoreGhcArgs` will fail to detect the
package ID to expose.

:::

With this small additional behaviour in place, Dyre detects that the
library is in a Cabal store and passes the extra CLI options to GHC.
GHC finds the library and compiles the program successfully:

```shell
% purebred
Configuration '/home/fraser/.config/purebred/purebred.hs' changed.
Recompiling.
Program reconfiguration successful.
Launching custom binary /home/fraser/.cache/purebred/purebred-linux-x86_64
… (realMain executes)
```

## Discussion

It is unfortunate that the author of a Dyre application must do
extra work to enable it to work with the Cabal store.  Even though
it is a small effort, it is something most programs will want to do.
I have so far been unable to conceive a robust solution that avoids
this work.

Dyre pre-0.9 did not have this feature.  To enable [Purebred][] to
work with the Cabal store, we performed the detection and
constructed the extra GHC options ourselves.  Then we passed the
extra options to Dyre via the [`ghcOpts`][ghcOpts] fields of the `Params` type.
This behaviour was ported to Dyre more or less *as-is* for the 0.9
release.  Except for the bit that I forgot to port, which was
restored in v0.9.1.

[ghcOpts]: https://hackage.haskell.org/package/dyre-0.9.1/docs/Config-Dyre.html#v:ghcOpts

My Purebred co-author reported a regression with Nix support in Dyre
0.9.x.  Under Nix, GHC fails to find the application library during
custom binary compilation.  I suspect the cause is an incidental
behavioural change as a result of porting the detection logic from
Purebred to Dyre.  But I have not yet started a proper
investigation.

[Purebred]: https://github.com/purebred-mua/purebred
