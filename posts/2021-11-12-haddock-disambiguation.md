---
tags: documentation
---

# Haddock: disambiguating types and values

Haskell has separate namespaces for types and values.  When types
and data constructors share a name, [Haddock][], Haskell's
documentation generator, can get confused.  [In this post I show how
to disambiguate types and values in Haddock
documentation.]{.abstract}

[Haddock]: https://haskell-haddock.readthedocs.io/en/latest/index.html

## Demonstrating the problem

For demonstration purposes I created a simple module, `ACME.Disamb`:

```haskell
module ACME.Disamb (Foo(..), Bar(..), Quux, Xyxxy) where

data Foo = Foo

-- | A bar contains a 'Foo'.  Example:
--
-- @let bar = 'Bar' 'Foo'@
--
data Bar = Bar Foo

data Quux

class Xyxxy a
```

Note that `Foo` is the name of both a type and a data constructor.
Same for `Bar`.  `Quux` is a type with no constructor and `Xyxxy` is
a class.  The Haddock
for type `Bar` contains ambiguous references to both `Bar` and
`Foo`.

Let's look at the HTML Haddock generated for each top-level
declaration:

### `data Foo`

```html
<div class="top">
  <p class="src">
    <span class="keyword">data</span> <a id="t:Foo" class="def">Foo</a>
    <a href="#t:Foo" class="selflink">#</a>
  </p>
  <div class="subs constructors">
    <p class="caption">Constructors</p>
    <table>
      <tbody>
        <tr>
          <td class="src"><a id="v:Foo" class="def">Foo</a></td>
          <td class="doc empty">&nbsp;</td>
        </tr>
      </tbody>
    </table>
  </div>
</div>
```

The element representing type `Foo` has `id="t:Foo"`, whereas the
constructor has `id="v:Foo"`.  These identifiers can be used as
fragment identifiers in hyperlinks.  Types and values are
disambiguated through the `t:…` and `v:…` identifier prefixes.

### `data Bar`

```html
<div class="top">
  <p class="src">
    <span class="keyword">data</span> <a id="t:Bar" class="def">Bar</a>
    <a href="#t:Bar" class="selflink">#</a>
  </p>
  <div class="doc">
    <p>
      A bar contains a
      <code><a href="ACME-Disamb.html#t:Foo" title="ACME.Disamb">Foo</a></code>. Example:
    </p>
    <pre>let bar = <code><a href="ACME-Disamb.html#t:Bar" title="ACME.Disamb">Bar</a></code> <code><a href="ACME-Disamb.html#t:Foo" title="ACME.Disamb">Foo</a></code></pre>
  </div>
  <!-- constructors elided -->
</div>
```

Here we can see that all references to `Foo` and `Bar` in the
documentation I wrote **all link to `t:Foo` or `t:Bar`**.  This is
not what I intended.  The usage example should refer to the data
constructors.

In my example this is a minor nuisance, but recall that `Foo` could
be the constructor of some other type.  The *type* `Foo` could be
unrelated!


### `data Quux`

```html
<div class="top">
  <p class="src">
    <span class="keyword">data</span> <a id="t:Quux" class="def">Quux</a>
    <a href="#t:Quux" class="selflink">#</a>
  </p>
</div>
```

`Quux` has no constructor.  As a result, there is no element with `id="v:…"`.

### `class Xyxxy a`

```html
<div class="top">
  <p class="src">
    <span class="keyword">class</span> <a id="t:Xyzzy" class="def">Xyzzy</a> a
    <a href="#t:Xyzzy" class="selflink">#</a>
  </p>
</div>
```

Type class names inhabit the type namespace.  Therefore the
corresponding element identifiers also use the `t:…` prefix.


## The solution

To refer explicitly to a type or value, prefix the reference with
`t` or `v`.  For example:

```haskell
-- | A bar contains a 'Foo'.  Example:
--
-- @let bar = v'Bar' v'Foo'@
--
data Bar = Bar Foo
```

The resulting HTML

```html
…
  <div class="doc">
    <p>
      A bar contains a
      <code><a href="ACME-Disamb.html#t:Foo" title="ACME.Disamb">Foo</a></code>. Example:
    </p>
    <pre>let bar = <code><a href="ACME-Disamb.html#v:Bar" title="ACME.Disamb">Bar</a></code> <code><a href="ACME-Disamb.html#v:Foo" title="ACME.Disamb">Foo</a></code></pre>
  </div>
…
```

::: note

This feature is available since [haddock-2.23.0][] ([commit][]).
The published [user guide][] is out of date but you can read
[up-to-date documentation on GitHub][doc-github].

[haddock-2.23.0]: https://hackage.haskell.org/package/haddock-2.23.0/changelog
[commit]: https://github.com/haskell/haddock/commit/dd47029cb29c80b1ab4db520c9c2ce4dca37f833
[user guide]: https://haskell-haddock.readthedocs.io/en/latest/index.html
[doc-github]: https://github.com/haskell/haddock/blob/haddock-2.25.0-release/doc/markup.rst#hyperlinked-identifiers

:::

## Inter-module references

Consider the following module:

```haskell
-- | See also 'ACME.Disamb.Quux'.
module ACME.Disamb2 where
```

Unlike references *within* a module, inter-module references default
to the *value* namespace:

```html
<p class="caption">Description</p>
<div class="doc">
  <p>
    See also
    <code><a href="ACME-Disamb.html#v:Quux" title="ACME.Disamb">Quux</a></code>.
  </p>
</div>
```

Recall that `Quux` has no constructor.  So the link doesn't even
target the wrong identifier; it targets a *non-existent* identifier.

The solution is the same: prefix the whole reference with `t`:

```haskell
-- | See also t'ACME.Disamb.Quux'.
module ACME.Disamb2 where
```
