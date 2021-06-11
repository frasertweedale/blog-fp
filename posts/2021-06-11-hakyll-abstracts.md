---
tags: hakyll, pandoc
---

# Generating abstracts for Hakyll articles

Suppose you have a list of recent posts and want to include an
abstract for each one.  Or maybe you want to include brief article
summaries in metadata about your content.  [In this post I
demonstrate several ways to declare or generate abstracts for
content on your [Hakyll][] site.]{.abstract}

[Hakyll]: https://jaspervdj.be/hakyll/

## Objective

The goal is to include an `$abstract$` field in each article's
context.  The field value should be a brief abstract or description
of the article.  What to actually *do* with the value is outside the
scope of this post.  But it is fair to include an example, so here's
how you could use it in a "recent posts" list:

```html
<ul>
  $for(posts)$
    <li><a href="$url$">$title$</a>: $abstract$</li>
  $endfor$
</ul>
```

I will discuss a more interesting use case in a future post.

## Metadata

Hakyll processes optional metadata at the top of the article source.
The format is YAML.  Fields in the YAML map are available via
[`metadataField`][metadataField], which is also part of the
[`defaultContext`][defaultContext].

[metadataField]: https://hackage.haskell.org/package/hakyll-4.14.0.0/docs/Hakyll-Web-Template-Context.html#v:metadataField
[defaultContext]: https://hackage.haskell.org/package/hakyll-4.14.0.0/docs/Hakyll-Web-Template-Context.html#v:defaultContext

So you can define an abstract in the metadata, like so:

```yaml
---
tags: hakyll, pandoc
abstract: >
  In this post I demonstrate several ways to generate
  abstracts for articles in your Hakyll site.
---

# Generating abstracts for Hakyll articles

…
```

::: note

Be careful of including HTML special characters (`&`, `<`, `>`, `"`,
`'`) in the metadata.  These will *not* be escaped automatically,
and could break the page.  I avoid this pitfall by escaping all
values that come from `metadataField`:

```haskell
context :: Context String
context =
  mapContext escapeHtml metadataField 
  <> …
```

:::

## Markup

I don't like repeating myself.  If I were to use `metadataField`,
the abstract I write would often be a repeat the article's
introduction or some part thereof.  Wouldn't it be nice if I could
just indicate—*inline*—a portion of the article to use as the
abstract?  For example:

```markdown
# Generating abstracts for Hakyll articles

Suppose you have … [In this post I demonstrate several
ways to generate abstracts for articles in your Hakyll
site.]{.abstract}

…
```

The example above uses Pandoc's `bracketed_spans` extension.  You
could achieve the same with explicit `<span>` tags.  Other input
formats may or may not provide a way to do it.

On the Hakyll side, we first need a function to locate a span with
the `abstract` class in the `Pandoc` AST:

```haskell
abstract :: Pandoc -> Maybe [Inline]
abstract (Pandoc _ blocks) =
  removeFormatting <$> findSpan blocks
  where
  findSpan = fmap getFirst . query $ \inl -> case inl of
    Span (_id, cls, _attrs) inls
        | "abstract" `elem` cls -> First (Just inls)
    _                           -> mempty
```

In the unlikely event that there are multiple spans with class
`abstract`, the `First [Inline]` monoid keeps only the first.  I
strip all formatting via [`removeFormatting`][removeFormatting],
which I [described][removeFormatting] in a previous post.

[removeFormatting]: 2021-01-11-hakyll-title-formatting.html#removeFormatting

The next step is to update the compiler to save a
snapshot of the abstract.
[`pandocCompilerWithTransformM`][pandocCompilerWithTransformM]
gives access to the `Pandoc` AST, and allows arbitrary
`Compiler` actions including `saveSnapshot`.

```haskell
match "posts/*" $ do
  route $ setExtension "html"
  compile $
    pandocCompilerWithTransformM
      defaultHakyllReaderOptions
      defaultHakyllWriterOptions
      (\pandoc -> do
        let render = fmap writePandoc . makeItem
                     . Pandoc mempty . pure . Plain
        maybe
            (pure ())
            (void . (saveSnapshot "abstract" <=< render))
            (abstract pandoc)
        pure pandoc
      )
    >>= loadAndApplyTemplate "templates/post.html" context
```

Finally we define a new kind of context field that can read
snapshots:

```haskell
snapshotField :: String -> Snapshot -> Context String
snapshotField key snap = field key $ \item ->
  loadSnapshotBody item snap
```

and add the field to the context:

```haskell
context :: Context String
context =
  snapshotField "abstract" "abstract"
  <> …
```

[pandocCompilerWithTransformM]: https://hackage.haskell.org/package/hakyll-4.14.0.0/docs/Hakyll-Web-Pandoc.html#v:pandocCompilerWithTransformM


## Autogeneration

Consider the following heuristic for autogenerating an abstract:
Take the first paragraph that immediately precedes a heading; that
is the abstract.

This is a very basic heuristic.  But absent other data it's probably
better than nothing.  So let's implement it:

```haskell
abstract :: Pandoc -> Maybe [Inline]
abstract (Pandoc _ blocks) =
  removeFormatting <$> fallback
  where
  fallback (Para inlines : Header _ _ _ : _) = Just inlines
  fallback (_h : t) = fallback t
  fallback [] = Nothing
```

This version of `abstract` scans the list of block elements at the
top level of the `Pandoc` AST.  The first time it sees a `Para`
preceding a `Header`, it returns the paragraph content.


## Putting it all together

For my sites, I want to use all three methods described above.  An
abstract specified in the *metadata* is preferred.  Explicit
*markup* is my second preference and the *autogeneration* heuristic
is a last resort.  This will provide a good user experience for me.
With a tiny bit of markup I can avoid repeating myself most of the
time.  But if it is warranted, I can use the metadata to write
something different.  Sometimes I'll get a fair result without doing
anything.

Combining the two versions of `abstract` is left as an exercise for
the reader (hint: `Control.Applicative.<|>`).

Take care when composing the context.  The `metadataField` has to
come before the `snapshotField` (if that's the priority you want):

```haskell
context :: Context String
context =
  mapContext escapeHtml metadataField
  <> snapshotField "abstract" "abstract"
  <> …
```

Now I have a nice way to generate abstracts for my articles.  I will
explore an interesting use case in an upcoming post.
