---
tags: hakyll
---

# Hakyll how-to: pages without source files

[Hakyll][] is a static website builder.  The typical use case is to
take some files written in lightweight markup and compile them into
static HTML.  Besides the sources for the main content, there are
also HTML templates, CSS, perhaps some JavaScript, images and so on.

But perhaps you need to build a site from sources other than local
files.  Possible scenarios include:

- Crawl and mirror another site
- Generate and publish calendars files for a conference
- Create a directory of content from some other source

As a static site builder, Hakyll can be a good choice for publishing
data that change infrequently.  [In this post I demonstrate how to
generate Hakyll site content without corresponding source
files.]{.abstract}

[Hakyll]: https://jaspervdj.be/hakyll/


## Use case: Web Key Directory

My use case was to generate a [*Web Key Directory (WKD)*][wkd] for
my personal OpenPGP keys.  OpenPGP clients can use WKD for key
discovery.  WKD is an alternative to the older keyserver system, which
[has some problems][].  It works as follows:

1. The client seeks a key for identity `Joe.Doe@example.org`.

2. Lower-case, digest (SHA-1) and [z-base-32][] encode the address
   local part (`Joe.Doe` → `iy9q119eutrkn8s1mk4r39qejnbu3n5q`).

3. The client performs HTTP GET request for:

   ```
   https://openpgpkey.example.org
     /.well-known/openpgpkey/example.org
     /hu/iy9q119eutrkn8s1mk4r39qejnbu3n5q?l=Joe.Doe
   ```

   This is called the *advanced method*.

4. The server responds with the binary encoding of the key (or a
   `404`).

5. As a fallback, the client tries:

   ```
   https://example.org
     /.well-known/openpgpkey
     /hu/iy9q119eutrkn8s1mk4r39qejnbu3n5q?l=Joe.Doe
   ```

   This is called the *direct method*.

Observe that, up to case-sensitivity of the email local part, the
server does not necessarily need to use the `?l=Joe.Doe` query
parameter.  If the local part is case-***in***sensitive (it usually is)
then the digest, which is part of the URI path, is enough.
Therefore an HTTP server serving static files can be a functional
WKD server.  You just need to build the directory.

[wkd]: https://wiki.gnupg.org/WKD
[has some problems]: https://gist.github.com/rjhansen/67ab921ffb4084c865b3618d6955275f
[z-base-32]: https://philzimmermann.com/docs/human-oriented-base-32-encoding.txt

::: note

The *advanced method* has several advantages over the *direct
method*.  Users or organisations with multiple domains can maintain
a single unified WKD, or delegate to an external service.  The
advanced method does not interfere with existing sites.  On the down
side, the `openpgpkey` DNS subdomain must be configured for each
domain.  The TLS requirement is hardly a disadvantage, because of
[ACME][].

[ACME]: https://en.wikipedia.org/wiki/Automated_Certificate_Management_Environment

:::

## Building a Web Key Directory with Hakyll

### Reading keys from GnuPG

[GnuPG][] is my OpenPGP client.  My Hakyll program invokes `gpg` to
list and extract keys.  Here are the types of the functions involved
(I will not detail their implementations):

```haskell
type KeyId = String
type Uid = (String, String) -- ^ local-part, domain

listKeys :: IO (L8.ByteString)
exportKey :: KeyId -> Uid -> Compiler (Item L8.ByteString)
hashLocalPart :: String -> String
extractUserIds :: L8.ByteString -> [Uid]
```

::: note

I used the [*typed-process*][hackage-typed-process] package to
execute GnuPG.  The `--with-colons` option is helpful for inspecting
keys and their user IDs.  You can use an `--export-filter` to select
the user ID(s) to export.  The man pages and [`DETAILS`][DETAILS]
file contain all the information you need for programmatic
interaction with GnuPG.

:::

[GnuPG]: https://gnupg.org/
[DETAILS]: https://github.com/gpg/gnupg/blob/master/doc/DETAILS
[hackage-typed-process]: https://hackage.haskell.org/package/typed-process


### Creating resources

Hakyll sites use [`match`][match] to generate compilation rules
for local resources.  For example:

```haskell
match "posts/*.md" $ do
  route $ setExtension "html"
  compile pandocCompiler
```

[match]: https://hackage.haskell.org/package/hakyll-4.14.0.0/docs/Hakyll-Core-Rules.html#v:match

But there are no local files to `match`.  Instead, we use
[`create`][create] to declare new resources.  These resources can be
routed to like any other, but we must synthesise the content.  The
following function, given a key ID and an email address, exports the
key from GnuPG and establishes the route.

```haskell
wkdUid :: KeyId -> Uid -> Rules ()
wkdUid keyId uid@(localPart, domain) = do
  let path = ".well-known/openpgpkey/" <> domain <> "/hu/"
             <> hashLocalPart localPart
  create [fromFilePath path] $ do
    route $ idRoute
    compile $ exportKey keyId uid
```

`create` is applied to a list of resource identifiers, and a `Rules
()` for compiling and routing the resource.  I am not showing the
implementation of `exportKey`; it's enough to know that it uses
`IO`.  Hakyll provides [`unsafeCompiler :: IO a -> Compiler
a`{.haskell}][unsafeCompiler] for executing I/O actions in compiler
context.

[create]: https://hackage.haskell.org/package/hakyll-4.14.0.0/docs/Hakyll-Core-Rules.html#v:create
[unsafeCompiler]: https://hackage.haskell.org/package/hakyll-4.14.0.0/docs/Hakyll-Core-Compiler.html#v:unsafeCompiler

`wkdUid` creates the key resource for a single user ID.  A key can
have multiple user IDs.  `wkUIDs on a specified KeydKey` creates the
Hakyll `Rules ()` for all user IDs on a given key:

```haskell
wkdKey :: KeyId -> Rules ()
wkdKey keyId = do
  out <- preprocess listKeys
  let uids = extractUserIds out
  traverse_ (wkdUid keyId) uids
```

`preprocess :: IO a -> Rules a`{.haskell} embeds an I/O action in
the `Rules` context.  Hakyll executes the action eagerly, unlike
`unsafeCompiler` which is run on demand.  This makes sense—we need
to `listKeys` first to work out what resources to create.

Having extracted the user IDs, we `traverse_` them to create the WKD
resources.  We also use `traverse_` at the outmost layer of the
program to invoke `wkdKey` for each key ID given on standard input:

```haskell
main :: IO ()
main = do
  keys <- words <$> getContents -- read stdin
  hakyll $ traverse_ wkdKey keys
```

## How Hakyll compiling works

Let's dive a bit deeper into how compiling works.  The
[`compile`][compile] function adds a content compiler to the `Rules`
for resources.  It's type is:

```haskell
compile
    :: (Binary a, Typeable a, Writable a)
    => Compiler (Item a)
    -> Rules ()
```

The compiler argument has to return an `Item a`, where `a` has
several type class constraints.  `Binary` and `Typeable` are used by
Hakyll's caching mechanism.  [`Writable`][Writable] is what Hakyll
uses to write the compiled resource to the site output.  There are
several instances, including for `String`, `ByteString` (lazy and
strict), and `Html`.  Hakyll provides `makeItem :: a -> Compiler
(Item a)` for returning an `Item a`.

As an example, here is a compiler that executes a subprocess and
returns the standard output:

```haskell
processCompiler
  :: String -> [String] -> Compiler (Item L.ByteString)
processCompiler exe args = do
  (_status, out, _err) <-
    unsafeCompiler . readProcess $ proc exe args
  makeItem out
```

[compile]: https://hackage.haskell.org/package/hakyll-4.14.0.0/docs/Hakyll-Core-Rules.html#v:compile
[Writable]: https://hackage.haskell.org/package/hakyll-4.14.0.0/docs/Hakyll-Core-Writable.html#t:Writable

## Conclusion

I have demonstrated how to use Hakyll to create content derived not
from local files, but other sources (GnuPG in my case).  Is this
actually a good idea?  For one-shot applications and data that don't
change frequently, I think it is fine.  Hakyll takes care of all the
filesystem I/O, relieving me of some tedium and avoiding a possible
source of bugs.

For my WKD use case, I can rebuild the site when my keys have been
updated, then `rsync` it to my web server.  The program is well
under 100 lines of code—including imports!  Source code is
[available on GitHub](https://github.com/frasertweedale/hakyll-wkd)
under AGPLv3.  At time of writing I have not published it on
Hackage.

::: note

Because there are no local file dependencies for the resources,
Hakyll does not know to recompile it.  You have to **`rebuild`**
every time.  This may be a practical issue for some use cases,
though not for mine.

:::

In general, it would be useful to be able to specify arbitrary
freshness checks for resources.  Unfortunately Hakyll's
implementation does not readily admit such a feature.  But it was
straightforward to add *unconditional* rebuilding on a per-resource
basis.  I submitted a [pull request][] to add the `forceCompile`
helper function.  Example usage:

```haskell
-- compile this resource every time
forceCompile $ create ["foo"] $ do
  route $ idRoute
  compile $ unsafeCompiler $ doStuff
```

[pull request]: https://github.com/jaspervdj/hakyll/pull/857

This feature was accepted and will appear in a future release of
Hakyll.
