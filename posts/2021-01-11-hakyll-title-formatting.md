---
tags: hakyll, pandoc
---

# Hakyll how-to: *Fancy* `title` <span style="font-variant:small-caps">formatting</span>

Sometimes you need special formatting in an article title.  I often
use monospace (e.g. for a function name), but subscript, superscript
or italics might be useful too.  The standard [Hakyll][] metadata
block doesn't offer a good way of doing this.  But Hakyll is very
flexible.  In this post I'll walk through my solution.

[Pandoc]: https://pandoc.org/
[Hakyll]: https://jaspervdj.be/hakyll/

## The standard approach

[Pandoc][] reads an optional YAML *metadata block* at the beginning of an
input file.  You specify the `title` there:

```markdown
---
title: Fancy Hakyll title formatting
tags: hakyll, pandoc
---

Sometimes you need…
```

Hakyll propagates these metadata fields in the
[`defaultContext`][defaultContext], so that you can refer to
`$title$` in the article template(s):

[defaultContext]: https://hackage.haskell.org/package/hakyll-4.13.4.1/docs/Hakyll-Web-Template-Context.html#v:defaultContext

```html
<h1>$title$</h1>

$body$
```

Typically you also include `$title$` in the HTML `<title>` element,
in the "top-level" template:

```html
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
    <title>pureblog - $title$</title>
    <link rel="stylesheet" type="text/css" href="/css/default.css" />
  </head>
  …
</html>
```

### Limitations

Say you want to use monospace for the "Hakyll" in the title.  Trying
with backticks…

```markdown
---
title: Fancy `Hakyll` title formatting
tags: hakyll, pandoc
---

Sometimes you need…
```

The site builds, but you end up with:

```html
…
  <head>
    <title>pureblog - Fancy `Hakyll` title formatting</title>
…
<h1>Fancy `Hakyll` title formatting</h1>
…
```

Not what we want.  The backticks were propagated verbatim, instead
of resulting in a `<code>` element.  Worse, when you have formatting
at the *beginning* of the value…

```markdown
---
title: *Fancy* Hakyll title formatting
tags: hakyll, pandoc
---

Sometimes you need…
```

Then the build fails with a YAML parse error:

```
ftweedal% cabal run site build
Up to date
Initialising...
  Creating store...
  Creating provider...
site: ./posts/2021-01-15-hakyll-title-formatting.md:
      YAML parse exception at line 2, column 13,
while scanning an alias:
did not find expected alphabetic or numeric character
  Running rules...
```

OK, what about using HTML tags in the metadata?

```markdown
---
title: <em>Fancy</em> <code>Hakyll</code> title formatting
tags: hakyll, pandoc
---

Sometimes you need…
```

The site builds successfully, and the resulting HTML is:

```html
…
  <head>
    <title>pureblog - <em>Fancy</em> <code>Hakyll</code> title formatting</title>
…
<h1><em>Fancy</em> <code>Hakyll</code> title formatting</h1>
…
```

The `<h1>` header is formatted exactly how we want.  But now there
are literal HTML tags in the `<title>` (browsers do not recognise
formatting here).  So we need a "plain" variant of the title to put
in the `<title>` element.  We may also need to use the plain variant
in Atom/RSS feeds, or other parts of the site.

Also, writing HTML is not nice.  A big reason to use Hakyll and
Pandoc is to write in a more fluent and human-friendly markup
format.  It is undesirable to have to revert to HTML.


## Two titles?

As demonstrated, two variants of the title are needed—one with
(optional) formatting, and one without.  Therefore we could write:

```markdown
---
title: Fancy Hakyll title formatting
fancyTitle: <em>Fancy</em> <code>Hakyll</code> title formatting
tags: hakyll, pandoc
---

Sometimes you need…
```

And update relevant templates to use `$fancyTitle$`:

```html
<h1>$fancyTitle$</h1>

$body$
```

This approach works.  But it is unsatisfying because not only are we
writing HTML; we are also writing the same title twice!  This fails
the DRY (*don't repeat yourself*) principle.  Even when `fancyTitle`
has formatting, the content is substantially similar.  In fact, you
could derive the plain variant from the other.

So that's what I do.

## Solution

First, promote the title out of the metadata block and into the
document itself, as a heading:

```markdown
---
tags: hakyll, pandoc
---

# *Fancy* `Hakyll` title formatting

Sometimes you need…
```

You'll need to update the the compilation rule to extract the first
header from the document, compute values for `$title$` and
`$fancyTitle$` and remember them.  Define the extraction function:

```haskell
firstHeader :: Pandoc -> Maybe [Inline]
firstHeader (Pandoc _ xs) = go xs
  where
  go [] = Nothing
  go (Header _ _ ys : _) = Just ys
  go (_ : xs) = go xs
```

And define the "strip formatting" function.  `removeFormatting` uses
[`Text.Pandoc.Walk.query`][query] to yield, in order, only the
"terminal" or "leaf" data from a `[Inline]`.  `query` monoidally
appends the values yielded by the inner function `f`.

[query]: https://hackage.haskell.org/package/pandoc-types-1.22/docs/Text-Pandoc-Walk.html#v:query

```haskell
removeFormatting :: [Inline] -> [Inline]
removeFormatting = query f
  where
  f inl = case inl of
    Str s -> [Str s]
    Code _ s -> [Str s]
    Space -> [Space]
    SoftBreak -> [SoftBreak]
    LineBreak -> [LineBreak]
    Math _ s -> [Str s]
    RawInline _ s -> [Str s]
    _ -> []
```

Next, update the Hakyll compilation rule to extract the header,
process and render its content, and save snapshots of the values.

```haskell
compile $ do
  -- BEGIN title processing
  pandoc <- readPandoc =<< getResourceBody
  let
    h1 = maybe [Str "no title"] id . firstHeader <$> pandoc
    render f =
      writePandoc . fmap (Pandoc mempty . pure . Plain . f)
    title = render removeFormatting h1
    fancyTitle = render id h1
  saveSnapshot "title" title
  saveSnapshot "fancyTitle" fancyTitle
  -- END title processing

  pandocCompiler
    >>= loadAndApplyTemplate "templates/post.html" ctx
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls
```

The final code change is to update `ctx` to retrieve values
for `$title$` and `$fancyTitle$` from the snapshots:

```haskell
ctx :: Context String
ctx =
  dateField "date" "%Y-%m-%d"
  <> snapshotField "title" "title"
  <> snapshotField "fancyTitle" "fancyTitle"
  <> defaultContext

snapshotField :: String -> Snapshot -> Context String
snapshotField key snap = field key $ \item ->
  loadSnapshotBody (itemIdentifier item) snap
```

Finally, update relevant templates.  The post template
(`templates/post.html`) does not refer to `$title$` or
`$fancyTitle$`; the title is now part of the document `$body$`:

```html
$body$
```

Other templates (e.g. the archive page, `templates/archive.html`)
can use the `$fancyTitle$`:

```html
Here you can find all my previous posts.

<ul>
  $for(posts)$
    <li>
      $date$ - <a href="$url$">$fancyTitle$</a>
    </li>
  $endfor$
</ul>
```

### Results

The resulting HTML has the plain `$title$` value in the `<title>`
element.  The formatted title appears as an `<h1>` element in the
article `$body$`.

```html
…
  <head>
    <title>pureblog - Fancy Hakyll title formatting</title>
…
<h1 id="fancy-hakyll-title-formatting"><em>Fancy</em>
  <code>Hakyll</code> title formatting.</h1>
…
```

The archive page uses `$fancyTitle$`, with the rich formatting, as
the link (`<a>`) text for each post:

```html
…
<ul>
  <li>
    2021-01-15 -
    <a href="./posts/2021-01-15-hakyll-title-formatting.html"><em>Fancy</em>
    <code>Hakyll</code> title formatting</a>
  </li>
  …
```

## Performance (what not to do)

It is critical for performance to extract and process the header
*during compilation*, saving snapshots of the computed values.  I
found this out the hard way.  In a previous implementation of my
solution I snapshotted the input file *source* during compilation:

```haskell
compile $ do
  getResourceBody >>= saveSnapshot "source"
  pandocCompiler >>= …
```

```haskell
headerField :: String -> ([Inline] -> [Inline]) -> Context String
headerField key f = field key $ \item -> do
  doc <- readPandoc =<< loadSnapshot (itemIdentifier item) "source"
  pure . itemBody . writePandoc
    $ Pandoc mempty . pure . Plain . f
      . maybe [Str "no title"] id . firstHeader
    <$> doc

ctx :: Context String
ctx =
  dateField "date" "%Y-%m-%d"
  <> headerField "title" removeFormatting
  <> headerField "fancyTitle" id
  <> defaultContext
```

With this implementation the build duration for my work blog (68
posts at the time) was around 34 seconds.  Certainly long enough to
irritate me.  When I investigated, I discovered that Hakyll executes
the process in `headerField`—load snapshot, parse with Pandoc,
extract title—every time it encountered `$title$` or `$fancyTitle$`
in a template.  That was 683 times, or over 10 times per post!

That seemed like an huge number to me, so I worked the numbers.
Each post page has its `<title>`, and also a list of 5 recent
articles.  Each article appears in the main archive list, and also
the list for each of its tags (I guess an average of 3 tags per
article).  And there are 5 recent articles on the home page.

```haskell
λ> import Data.Monoid (Sum(..))
λ> titles = id
λ> recent = (*5)
λ> archive = id
λ> tags = (*3)
λ> index = const 5
λ> mconcat [titles, recent, archive, tags, index] $ Sum 68
Sum {getSum = 685}
```

The estimate (685) was very close to the actual value (683).
Although this is the worst case scenario, even the best case
scenario—when nothing has changed at all—still caused 192 parses and
took significant time (about 9 seconds).  This seems to be caused by
tags pages always being recompiled.  I'm not sure why that happens,
but when the titles are cached it's fast enough to not be an issue
(<0.5 seconds).

The big takeaway from all this is: do as little processing as
possible in `Context` field definitions.  In my case, switching to
doing the title processing in `compile` and caching the results
([`saveSnapshot`][saveSnapshot]) reduced site build time from 34
seconds to 9 seconds!

[saveSnapshot]: https://hackage.haskell.org/package/hakyll-4.13.4.1/docs/Hakyll-Core-Compiler.html#v:saveSnapshot


## Final words

Pandoc and Hakyll continue to be awesome and powerful tools.  Any
time I've wished to do something a little differently, or enhance my
site, I've always found a way (up to HTML and CSS, anyway).

I did confirm that my solution results in 1 additional parse of each
article source (2 parses in total).  Using
[`pandocCompilerWithTransformM`][pandocCompilerWithTransformM] I
might be able to do the title processing in a callback, so that each
article source gets parsed exactly once.  This would further reduce
the build time.  I left this as a future improvement (and an
exercise for the reader).

[pandocCompilerWithTransformM]: https://hackage.haskell.org/package/hakyll-4.13.4.1/docs/Hakyll-Web-Pandoc.html#v:pandocCompilerWithTransformM

Earlier I did omit one important detail for the sake of clarity.
The "recent posts" list accompanying each article requires an
additional `Rules` set, creating a `"recent"` version of each
article `Item` that can be referenced in the "main" article.

```haskell
match "posts/*" $ version "recent" $ do
  …
```

It is in *this* `Rules` set that I extract and snapshot the titles.
Accordingly, in `snapshotField` I have to explicitly (and
unconditionally) request the snapshot from the `"recent"` version of
the current `Item`.

```haskell
snapshotField :: String -> Snapshot -> Context String
snapshotField key snap = field key $ \item ->
  let
    ident = itemIdentifier item
    ident' = setVersion (Just "recent") ident
  in
    loadSnapshotBody ident' snap
```
