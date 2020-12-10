----
tags: hakyll, pandoc
----

# Adding section links to Hakyll articles with `Text.Pandoc.Walk`

It is handy to be able to link to section headings in long articles.
[Pandoc][] and [Hakyll][] don't do that out of the box.  But they
give you all the power you need to implement it yourself.  In this
post I'll show you how.

[Pandoc]: https://pandoc.org/
[Hakyll]: https://jaspervdj.be/hakyll/


## Objective

The main objective is to provide links (HTML `<a>` element) to
section headings.  They should be located near or within the heading
element.  Having them in the document will make it easy for readers
to grab a link to a specific section of the article.  (I myself
often want to do this!)

You could make the whole heading a link, but I like the approach
that reveals a link when the pointer hovers over the heading.  Some
sites use a pilcrow (¶), pound (#) or a link symbol (🔗) as the link
text.  I will use a section sign (§).


## Building blocks

Pandoc does set the `id` attribute of HTML heading elements to a
value derived from the heading text.  For example, one of my
previous posts had a section headed [Probabilities][].  The
resulting HTML for the heading is:

```html
<h2 id="probabilities">Probabilities</h2>
```

[Probabilities]: 2020-03-31-quickcheck-hedgehog.html#probabilities

The value of the `id` attribute will be the `href` target of the
`<a>` element we create (with `#` prepended to make it a URI
fragment).

Hakyll provides the `pandocCompilerWithTransform` function for
compiling documents using Pandoc and applying an arbitrary
transformation to them.  It has the type:

```haskell
pandocCompilerWithTransform
  :: ReaderOptions
  -> WriterOptions
  -> (Pandoc -> Pandoc)
  -> Compiler (Item String)
```

Note the `(Pandoc -> Pandoc)` argument.  This is the tranformation
function.  It works with the [`Pandoc`][] native AST data type,
rather than HTML or the input type (e.g. Markdown).

[`Pandoc`]: https://hackage.haskell.org/package/pandoc-types-1.21/docs/Text-Pandoc-Definition.html#t:Pandoc

For constructing such a transformation, Pandoc provides the
[`Text.Pandoc.Walk`][] module and the `walk` function:

```haskell
walk :: (Walkable a b) => (a -> a) -> b -> b
```

`walk f x` walks the structure `x :: b` (bottom up) and replaces
every occurrence of a value of type `a` with the result of applying
`f :: (a -> a)` to it.

There are many instances of `Walkable`.  We are interested in the
one that visits all the `Block` elements (that's what headings are)
in the `Pandoc`:

```haskell
instance Walkable Block Pandoc
```

[`Text.Pandoc.Walk`]: https://hackage.haskell.org/package/pandoc-types-1.21/docs/Text-Pandoc-Walk.html#v:walk


## Putting it together

I needed a handful of Pandoc constructors to implement the
transformation.  The `Header` constructor (of the `Block` data type)
represents a document (sub)heading with `Int` depth, attributes, and
the list of `Inline` elements that constitute the header content.

```haskell
data Block
  ...
  | Header Int Attr [Inline]
  ...
```

I also had to construct a `Link` (one of the cases of the `Inline`
data type).  A `Link` has attributes, content (`[Inline]`) and a
target.  I also used the `Str` and `Space` constructors.

```haskell
data Inline
    = Str Text              -- ^ Literal text
    ...
    | Space                 -- ^ Inter-word space
    ...
    | Link Attr [Inline] Target
```

By the way, `Attr` and `Target` are defined as:

```haskell
-- id, classes and key-value pairs
type Attr = (Text, [Text], [(Text, Text)])

-- URI, title
type Target = (Text, Text)
```

With these constructors in hand, here is the whole transformation
function:

```haskell
addSectionLinks :: Pandoc -> Pandoc
addSectionLinks = walk f where
  f (Header n attr@(idAttr, _, _) inlines) | n > 1 =
    let link = Link nullAttr [Str "§"] ("#" <> idAttr, "")
    in Header n attr (inlines <> [Space, link])
  f x = x
```

Note that we only apply this change to headings of depth greater
than one.  I do not need to provide a link for the article title,
which is at the top of the page.  For all other headers, we add a
`Link` to its inline content, where the target is the fragment
pointing at the `idAddr` of the header itself.

To apply the transformation, I had to replace a single occurrence
of:

```haskell
pandocCompiler :: Compiler (Item String)
```

with:

```haskell
pandocCompilerWithTransform
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
        addSectionLinks
```


## Style

I want to hide the heading link unless the cursor is hovering over
the heading.  I would like to accomplish it with this small dose of
CSS:

```css
:is(h2, h3, h4, h5, h6) a {
    text-decoration: none;
    color: grey;
    visibility: hidden;
}

:is(h2, h3, h4, h5, h6):hover a {
    visibility: visible;
}
```

The `is()` pseudo-class function matches anything that matches the
selector arguments, avoiding tedious repetition.  It is part of CSS
[Selectors Level 4][], which is still a draft.  Firefox and Safari
fully support it but unfortunately other browsers are [lagging
behind][].  So I am stuck with the tedious repetition for now:

```css
h2 a, h3 a, h4 a, h5 a, h6 a {
    text-decoration: none;
    color: grey;
    visibility: hidden;
}

h2:hover a, h3:hover a, h4:hover a, h5:hover a, h6:hover a {
    visibility: visible;
}
```

[Selectors Level 4]: https://www.w3.org/TR/selectors-4/#matches
[lagging behind]: https://caniuse.com/css-matches-pseudo

I also used `text-decoration` and `color` to make the link
appearance clean and understated.


## Conclusion

As a result of this change, the HTML emitted for section headers (of
depth > 1) looks like:

```html
<h2 id="probabilities">
  Probabilities <a href="#probabilities">§</a>
</h2>
```

You can experience the results for yourself, right here on this page
(and in my other posts).  You can also [view the commit][] that
implements this feature.

[view the commit]: https://github.com/frasertweedale/blog-fp/commit/fe72af9144fea3ece5295ac5446f647560119088

There's not much else to say, really.  Pandoc is still awesome.
Hakyll is still awesome.  And I am very happy with the results of
this little enhancement.  Go forth and pilcrow-ise your Hakyll
sites!
