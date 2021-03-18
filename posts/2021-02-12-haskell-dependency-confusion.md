---
tags: security, cabal, hackage
---

# Haskell is vulnerable to dependency confusion

In this post, I demonstrate that critical parts of the Haskell
package management system are vulnerable to the *dependency
confusion* supply chain attack.  I also discuss some potential
approaches for Haskell tooling to mitigate this type of attack.

## Introduction

A recent post, [*Dependency Confusion: How I Hacked Into Apple,
Microsoft and Dozens of Other Companies*][depcon] by Alex Birsan,
caused quite a stir.  It describes an attack where the attacker
uploads malicious packages with the same name as target
organisations' *internal* packages.  If the version on the public
repository is higher, most tools will prefer it.  The malicious
package compromises the developer's machine, or the machines that
run the program the developer builds, or the users who interact with
that program over the network.  Maybe all three.

[depcon]: https://medium.com/@alex.birsan/dependency-confusion-4a5d60fec610

The author successfully executed the dependency confusion attacks
using `npm` (NodeJS), PyPI/`pip` (Python) and RubyGems/`gem` (Ruby).
In the conclusion, he raises the spectre of other languages and
package repositories being likewise vulnerable:

> Specifically, I believe that finding new and clever ways to leak
> internal package names will expose even more vulnerable systems,
> and looking into alternate programming languages and repositories
> to target will reveal some additional attack surface for
> dependency confusion bugs.

So, is the Haskell ecosystem vulnerable to this attack?  I assume
you read the post title or abstract and therefore know that the
answer to that question is *yes!*  In the following sections I
demonstrate the attack and discuss possible mitigations.

## Terminology

- ***hackage-server***: the server program for running a
  Haskell package repository.

- ***Hackage***: in isolation, refers to the Haskell community's
  central package repository.  It runs *hackage-server* and is
  located at [**`hackage.haskell.org`**][hackage.haskell.org].

- ***Cabal***: the name of the Haskell package description
  format, and a library for working with such packages.

- ***cabal-install***: the command line program for building and
  installing *Cabal* packages.  The executable name is **`cabal`**.
  *cabal-install* uses the *Cabal* library and interacts with
  *hackage-server* servers (most commonly `hackage.haskell.org`).

- ***Private Hackage***: a *hackage-server* instance operated for
  the purpose of hosting private or internal packages.

[hackage.haskell.org]: https://hackage.haskell.org/


## Preparation

### Private Hackage repository

For reasons unrelated to this investigation, I already had a local
instance of *hackage-server*, running on `localhost:8080`.  In this
exercise, it plays the role of the private Hackage repository.

### Subject dependency

I created a new "internal package" to be the subject of the attack.
I chose a silly name suggested by GitHub: *redesigned-carnival*.
Hopefully it will not conflict with anyone's legitimate needs.
While the [source code][redesigned-carnival-github] is available on
GitHub, there's no need to look at it.  The package is trivial,
containing a single module whose content is:

```haskell
module ACME.RedesignedCarnival where
hello = "Hello, world!"
```

[redesigned-carnival-github]: https://github.com/frasertweedale/redesigned-carnival

I uploaded `redesigned-carnival-0.1.0.0` to the private Hackage
repository.

### *cabal-install* configuration

The *cabal-install* configuration lives at `~/.cabal/config`.  By
default it contains a single `repository` clause, pointing at
Hackage:

```
repository hackage.haskell.org
  url: http://hackage.haskell.org/
```

I added a second `repository` clause, pointing at my local Hackage
server:

```
repository localhost
  url: http://localhost:8080/
```

With the `repository` configured, I proceeded to update the local
package index via `cabal update`.

::: note

Due to a bug in the [*HTTP*][HTTP-hackage] package, updating the
index fails when the repository URL specifies a port.  The
workaround is to tell *cabal-install* to use a different HTTP
transport.  Set the `http-transport` option in `~/.cabal/config`:

```
-- valid values: curl, wget, powershell
http-transport: curl
```

I submitted a [pull request][HTTP-pr] to fix this issue.

:::

[HTTP-hackage]: https://hackage.haskell.org/package/HTTP
[HTTP-pr]: https://github.com/haskell/HTTP/pull/133

```shell
% cabal update
Downloading the latest package lists from:
- localhost
- hackage.haskell.org
To revert to previous state run:
    cabal v2-update 'hackage.haskell.org,2021-02-08T02:13:51Z'
```

We can see that the `localhost` index was retrieved.
*cabal-install* is now aware of packages in my private Hackage
server:

```shell
% cabal list redesigned-carnival
* redesigned-carnival
    Synopsis: Package for dependency confusion
    Default available version: 0.1.0.0
    Installed versions: [ Not installed ]
    License:  PublicDomain
```

### Vulnerable dependent package

I wrote a tiny package called *super-fiesta* (cheerful suggestion
thanks to GitHub, again).  It depends on *redesigned-carnival*, and
prints the value of `hello`.  The [source code][super-fiesta-github]
is on GitHub.  Here is the whole program (`Main.hs`):

[super-fiesta-github]: https://github.com/frasertweedale/super-fiesta

```haskell
module Main where

import ACME.RedesignedCarnival (hello)

main :: IO ()
main = putStrLn hello
```

With *cabal-install* now aware of *redesigned-carnival*, I can build
and run this program:

```shell
% cd ~/dev/hs/super-fiesta
% cabal run
Resolving dependencies...
Downloading  redesigned-carnival-0.1.0.0
Downloaded   redesigned-carnival-0.1.0.0
Build profile: -w ghc-8.8.4 -O1
In order, the following will be built (use -v for more details):
 - redesigned-carnival-0.1.0.0 (lib) (requires build)
 - super-fiesta-0.1.0.0 (exe:super-fiesta) (first run)
Starting     redesigned-carnival-0.1.0.0 (lib)
Building     redesigned-carnival-0.1.0.0 (lib)
Installing   redesigned-carnival-0.1.0.0 (lib)
Completed    redesigned-carnival-0.1.0.0 (lib)
Configuring executable 'super-fiesta' for super-fiesta-0.1.0.0..
Preprocessing executable 'super-fiesta' for super-fiesta-0.1.0.0..
Building executable 'super-fiesta' for super-fiesta-0.1.0.0..
[1 of 1] Compiling Main             ( Main.hs, /home/ftweedal/dev/hs/super-fiesta/dist-newstyle/build/x86_64-linux/ghc-8.8.4/super-fiesta-0.1.0.0/x/super-fiesta/build/super-fiesta/super-fiesta-tmp/Main.o )
Linking /home/ftweedal/dev/hs/super-fiesta/dist-newstyle/build/x86_64-linux/ghc-8.8.4/super-fiesta-0.1.0.0/x/super-fiesta/build/super-fiesta/super-fiesta ...
Hello, world!
```

### Determining internal package names

The attack requires knowing, or guessing, internal package names.
Approaches that apply to Haskell code include:

- Search for mentions of internal libraries in public code, blog
  posts, presentations, and so on.

- Scan non-stripped binaries delivered by the target organisation.

- If there is an organisation-specific package naming convention,
  make some educated guesses.

- Use any other kind of exploit (including social engineering) to
  discover internal package names.

I will assume that the hypothetical attacker has learned, somehow,
that I have an internal package named *redesigned-carnival*, and
will attempt to use dependency confusion to attack me.  This is an
appropriate assumption.

## Attack

I bumped the version of *redesigned-carnival* to `1.0.0.0`, and
[uploaded it to Hackage][r-c-hackage].  A real-world attacker might
upload *many* different versions, in the hope of matching a range of
possible constraints.

[r-c-hackage]: https://hackage.haskell.org/package/redesigned-carnival

The version uploaded to Hackage does not contain any malicious
behaviour.  But I changed the `hello` string to `"Mwahaha"`.  If we
see this sinister-looking string, we'll know the attack succeeded.

After another `cabal update`, *cabal-install* is aware of the new
version of *redesigned-carnival*:

```shell
% cabal list redesigned-carnival
* redesigned-carnival
    Synopsis: Package for dependency confusion
    Default available version: 1.0.0.0
    Installed versions: [ Not installed ]
    License:  PublicDomain
```

The *Default available version* line shows that, by default,
*cabal-install* will prefer the highest version of package.  To
confirm that it was not merely the *most recently uploaded* version
of a package, I created version `0.2.0.0`, uploaded it to my
*private* Hackage server, and ran `cabal update`.  `cabal list`
still showed version `1.0.0.0` as the default version.

This is what *cabal-install* did the next time I built (and ran)
*super-fiesta*:

```shell
% cabal run
Resolving dependencies...
Build profile: -w ghc-8.8.4 -O1
In order, the following will be built (use -v for more details):
 - redesigned-carnival-1.0.0.0 (lib) (requires build)
 - super-fiesta-0.1.0.0 (exe:super-fiesta) (configuration changed)
Starting     redesigned-carnival-1.0.0.0 (lib)
Building     redesigned-carnival-1.0.0.0 (lib)
Installing   redesigned-carnival-1.0.0.0 (lib)
Completed    redesigned-carnival-1.0.0.0 (lib)
Configuring executable 'super-fiesta' for super-fiesta-0.1.0.0..
Preprocessing executable 'super-fiesta' for super-fiesta-0.1.0.0..
Building executable 'super-fiesta' for super-fiesta-0.1.0.0..
[1 of 1] Compiling Main             ( Main.hs, /home/ftweedal/dev/hs/super-fiesta/dist-newstyle/build/x86_64-linux/ghc-8.8.4/super-fiesta-0.1.0.0/x/super-fiesta/build/super-fiesta/super-fiesta-tmp/Main.o ) [ACME.RedesignedCarnival changed]
Linking /home/ftweedal/dev/hs/super-fiesta/dist-newstyle/build/x86_64-linux/ghc-8.8.4/super-fiesta-0.1.0.0/x/super-fiesta/build/super-fiesta/super-fiesta ...
Mwahaha
```

Owned.  *cabal-install* saw that there was a new version of
*redesigned-carnival* compatible with the constraints (because there
are no constraints).  So it downloaded, built, installed and linked
to the "malicious" version from Hackage.

The order the repositories appear in `~/.cabal/config` does not
affect the result.  My testing shows that *cabal-install* prefers
the highest version, regardless of which repository it comes from.
I have seen comments that repositories listed later are preferred
over earlier ones, but maybe that only applies when the same version
is hosted on both repositories.


## Mitigations

Alternative workflows and tools can mitigate the risk of dependency
confusion attacks.  I discuss some current approaches below, and also
some ideas for future tooling enhancements.

### Local packages (effective)

Instead of using a private Hackage server, you can specify local
packages in `cabal.project` or `cabal.project.local`:

```shell
% echo "packages: ., ../redesigned-carnival" > cabal.project.local
% cabal run exe:super-fiesta
Resolving dependencies...
Build profile: -w ghc-8.8.4 -O1
In order, the following will be built (use -v for more details):
 - redesigned-carnival-0.1.0.0 (lib) (first run)
 - super-fiesta-0.1.0.0 (exe:super-fiesta) (configuration changed)
Configuring library for redesigned-carnival-0.1.0.0..
Preprocessing library for redesigned-carnival-0.1.0.0..
Building library for redesigned-carnival-0.1.0.0..
[1 of 1] Compiling ACME.RedesignedCarnival ( src/ACME/RedesignedCarnival.hs, /home/ftweedal/dev/hs/super-fiesta/dist-newstyle/build/x86_64-linux/ghc-8.8.4/redesigned-carnival-0.1.0.0/build/ACME/RedesignedCarnival.o )
Configuring executable 'super-fiesta' for super-fiesta-0.1.0.0..
Preprocessing executable 'super-fiesta' for super-fiesta-0.1.0.0..
Building executable 'super-fiesta' for super-fiesta-0.1.0.0..
[1 of 1] Compiling Main             ( Main.hs, /home/ftweedal/dev/hs/super-fiesta/dist-newstyle/build/x86_64-linux/ghc-8.8.4/super-fiesta-0.1.0.0/x/super-fiesta/build/super-fiesta/super-fiesta-tmp/Main.o ) [ACME.RedesignedCarnival changed]
Linking /home/ftweedal/dev/hs/super-fiesta/dist-newstyle/build/x86_64-linux/ghc-8.8.4/super-fiesta-0.1.0.0/x/super-fiesta/build/super-fiesta/super-fiesta ...
Hello, world!
```

Local packages are always preferred over versions in repositories,
even when a repository offers a higher version.  You can use manual
cloning, Git submodules or a monorepo to get the internal package
sources in the right place.

A minor downside to this approach is that you have to be more
explicit about which package you want to build, run or install.  In
the transcript above, I executed `cabal run exe:super-fiesta`.
If I omit the final argument, *cabal-install* complains:

```shell
% cabal run
Resolving dependencies...
TODO: add support for multiple packages in a directory
CallStack (from HasCallStack):
  error, called at ./Distribution/Client/ProjectOrchestration.hs:542:9 in main:Distribution.Client.ProjectOrchestration

```

### `cabal freeze` (effective, with caveats)

Dependency version pinning via `cabal freeze` is not an effective
mitigation *in general*.  To demonstrate, I uploaded
`redesigned-carnival-0.3.0.0` to my private Hackage, and the same
version (with "malicious" modification) to `hackage.haskell.org`.  I
executed `cabal freeze` and modified the `cabal.project.freeze` file
to require `redesigned-carnival-0.3.0.0`:

```shell
% cat cabal.project.freeze
constraints: any.base ==4.13.0.0,
             any.ghc-prim ==0.5.3,
             any.integer-gmp ==1.0.2.0,
             any.redesigned-carnival ==0.3.0.0,
             any.rts ==1.0
```

Then I executed `cabal run`.  With both repositories offering the
"same" version, *cabal-install* retrieves the package from the *last
defined* repository.  If that is `hackage.haskell.org`, then you
will download and build the malicious variant.

So in the presence of public and private Hackage repositories, the
security of dependency freezing depends on the repository order.
Unfortunately the official *cabal-install*
[documentation][cabal-doc-repository] fails to mention the
significance of repository order:

> If you want, you can configure multiple repositories, and cabal
> will combine them and be able to download packages from any of
> them.

[cabal-doc-repository]: https://cabal.readthedocs.io/en/3.2/installing-packages.html#repository-specification

The vagueness of the documentation means insecure configurations are
likely.  If you work in a Haskell team inside a company, *everyone*
needs to get this configuration right.


### Use Nix (effective)

[Nix][] is a content-address package management system where all
packages pin all dependencies by their cryptographic hashes.  Many
Haskell developers and teams use it, and enjoy certainty about their
dependencies and reproducible builds.

Nix has a steep learning curve, and it is not a complete solution
for library and program authors who want to publish to Hackage.
*"Use Nix"* is a valid way to mitigate dependency confusion risk
for Haskell teams and some individuals.

[Nix]: https://nixos.org/


### Use Stack (effective?)

According to the results of annual Haskell developer surveys
([2020][], [2019][], [2018][], [2017][]), around 75% of Haskell
developers use the [Stack][] build tool.  Stack, by default, uses
curated package sets from [Stackage][].  For internal packages it
allows pinning to exact Git commit hashes, and local packages.

[2020]: https://taylor.fausak.me/2020/11/22/haskell-survey-results/#s3q0
[2019]: https://taylor.fausak.me/2019/11/16/haskell-survey-results/#s3q0
[2018]: https://taylor.fausak.me/2018/11/18/2018-state-of-haskell-survey-results/#question-041
[2017]: https://taylor.fausak.me/2017/11/15/2017-state-of-haskell-survey-results/#question-22
[Stack]: https://haskellstack.org/
[Stackage]: https://www.stackage.org/

On the face of it, it would seem that Stack, in a standard
configuration, protects against dependency confusion attacks.  I
don't use Stack and I haven't tested it as part of this
investigation.  I'll give it the benefit of the doubt and hope that
others might put it to the test and publish their findings.

Even if Stack mitigates dependency confusion perfectly, according to
the same surveys above, around 50% of Haskell developers use
*cabal-install* (either exclusively or alongside other build tools).
As was the case with Nix, Stack may be a solution for particular
teams or individuals, but it is not a solution for the whole
community.


### "Exclusive" repositories (~~idea~~ effective, with caveats)

If you could mark a repository as "exclusive", then for any package
provided by that repository, *cabal-install* **must** only use a
version provided by that repository.  The idea is that private
repositories would be marked as exclusive:

```
repository localhost
  url: http://localhost:8080/
  exclusive: True

repository hackage.haskell.org
  url: http://hackage.haskell.org/
```

Now, a dependency confusion attack against *redesigned-carnival*
would not work.  *cabal-install* will ignore versions of the package
from `hackage.haskell.org`, because that package name is also
provided by an exclusive repository.

This approach is easy for users to configure.  But, as with the
`cabal freeze` approach, security for organisations requires every
developer to set the configuration properly.

and requires no
changes to the *hackage-server* program or the *Cabal* package
description format.  The changes are limited to *cabal-install*.

One drawback is that if a person or organisation wants to publicly
release a previously internal package, they will have to remove it
from their private repository.  As far as I can tell,
*hackage-server* does not support package deletion.  So this
approach might entail changes to *hackage-server* after all.

#### *Update*: coming in *cabal-install* 3.4

Oleg Grenrus [pointed out][reddit-cabal-3.4] a new feature arriving
in `cabal-install-3.4`.  The `active-repositories` field and
`override` merge strategy can accomplish the goal of restricting
packages to a particular repository:

```
-- Order is significant.  For packages in localhost,
-- only versions in localhost are considered
active-repositories:
  , hackage.haskell.org
  , localhost:override
```

See also the [official documentation][cabal-3.4-doc].  I have tested
the feature, and it works.  At time of writing the pre-release
version is `3.4.0.0-rc7` so the final release should not be far
away.

[reddit-cabal-3.4]: https://www.reddit.com/r/haskell/comments/lhmbw3/haskell_is_vulnerable_to_dependency_confusion/gmz6qi0/
[cabal-3.4-doc]: https://cabal.readthedocs.io/en/3.4/cabal-project.html?highlight=active-repositories#cfg-field-active-repositories


### Repository-scoped dependencies (idea)

Dependencies in `.cabal` files have no scoping or namespacing.  What
if you could scope a dependency to a particular repository?  For
example:

```cabal
executable super-fiesta
  main-is: Main.hs
  build-depends:
    base >=4 && <5
    , <repository>:redesigned-carnival
  default-language:    Haskell2010
```

You could optionally constrain particular dependencies to come from
a specified repository.  This would be a more invasive change,
involving a change to the package description format.  *Cabal* (the
library) and *cabal-install* would be impacted.

To thwart attacks where people get confused or tricked into using
the wrong repository, everyone would need to agree on the names of
repositories (which are currently configured in `~/.cabal/config`).
In other words, every agent would need to agree on what
`<repository>` means.  This could be accomplished by identifying
repositories by URI, but there could be other valid approaches.

### Validated namespaces for packages (idea)

Another possible approach is that taken by the Java [*Maven*][maven]
system.  Packages are addressed by `<Group ID>:<Artifact
ID>:<Version>`, where `group-id` is based on a DNS name.  In order
to publish a new artifact to the [*Maven Central*][central] public
repository, the prospective publisher has to prove "control" of the
Group ID.  Most often this is done by publishing a particular DNS
TXT record in the domain from which the Group ID is derived.  But
some Group IDs follow other validation rules (e.g. commit access for
`org.github.*`.  Brian Fox's blog post [*Why Namespacing Matters in
Public Open Source Repositories*][fox] explains it well, and in more
detail.

[maven]: https://en.wikipedia.org/wiki/Apache_Maven
[central]: https://search.maven.org/
[fox]: https://blog.sonatype.com/why-namespacing-matters-in-public-open-source-repositories

Summary: if packages in public repositories are namespaced by
*something*, and if that *something* can be validated by the
repository before publishing, then the system is resistant to
dependency confusion attacks (up to spoofing the validation
mechanism).  DNS-based namespaces strike a fair balance between
verifiability and usability (for both publishers and users).

So, what would this look like in the Haskell world?  Retrofitting
our package ecosystem with namespaces would be a *very* disruptive
change.  All tooling would be affected.  In particular,
*hackage-server* would need to learn how to validate package
namespaces.

No doubt many people will be annoyed that they must now write
`haskell.org:base` (instead of just `base`),
`frase.id.au:redesigned-carnival`, and so on.  (By the way, those
namespace values are just a suggestion of a possible scheme).

Migrating existing packages poses several challenges.  Sure, put
core libraries in the `haskell.org` (or whatever) namespace.  But
where should *lens* live?  Who decides?  What if there is
disagreement?  And what happens to packages that don't have
responsive maintainers?

I think that with careful planning a smooth transition is possible.
To give people time to adjust, packages could for some period remain
accessible via namespaced and non-namespaced names.  Duplicate names
in different namespaces would be forbidden during the transition
period.  We can build tools to update `.cabal` files with the
namespaces, removing most of the pain for maintainers.

After the transition period ends, all the Haskell tooling will use
namespaced packages.  Importantly, users and developers do not need
to configure anything to be safe from dependency confusion attacks.
Only publishers will face some additional validation steps.

Apart from security I see another possible benefit to this approach.
Organisation-namespaced packages could give more visibility into
which organisations are using Haskell.  This could promote the
perception of Haskell as a language of industry.  *"Look, we are
like Java"* is a legitimate selling point for many people.

## Other attacks

Dependency confusion is just one kind of software supply chain
attack.  The approaches I discussed mitigate this specific attack.
But they do not protect against other attacks, such as an attacker
acquiring Ed Kmett's Hackage credentials and uploading a malicous
new release of *lens*.  Or the maintainer themselves turning evil.
Different protections are needed for other kinds of attacks.

Also, dependency confusion on its own is just a vector for malware.
Does the malware target the developer's machine, or the machines
that run the program the developer is building, or network clients?
All of the above?  The range of possible attacks depends on the
language, development environment and deployment environment.  The
different ways in which a Haskell dependency confusion victim could
actually be exploited is a topic for a whole other blog post.

These are all important topics to consider in the design of
languages, build tooling and package management systems.  In
additional to known supply chain attacks, human factors (social
engineering) should not be ignored either.

## Conclusion

In this post I demonstrated that the standard Haskell development
tooling—Hackage and Cabal/*cabal-install*—is vulnerable to
dependency confusion attacks.  I discussed some mitigations that are
feasible today, and some ideas for tool improvements that vary in
complexity and usability.  Finally I emphasised that dependency
confusion is one of several types of supply chain attack, and just
one small panel in the tapestry of software security.

I think that Haskell has long, *long* way to go in terms of
security.  Sure, the language itself is mostly pretty good.  But the
compiler, build tools, and package repositories all fall far short
when it comes to security.  I will have more to say about this in
future blog posts.

In the meantime, I hope this post kick-starts a discussion about how
we should address the particular threat of dependency confusion.
