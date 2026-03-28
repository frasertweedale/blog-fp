---
tags: hakyll
---

# Generating Hakyll pages from Haskell source

In typical use, Hakyll programs convert external content files into
HTML pages.  But what if your content is defined in Haskell source
files that are part of the Hakyll program?  That can work—but you
need a trick or two.  In this post I'll show you how.

## The scenario

For whatever reason, you've got some Haskell data structure you want
to present somehow.  A typical example would be a list or table or
tree of data.  My use case was a list of objects describing some
files, to be presented as a table.  A file may or may not be
available for download (`Maybe FilePath`).

```haskell
module Files where

data File = File
  { fileDate :: String
  , fileName :: Maybe FilePath
  , fileDesc :: String
  }

fileList :: [File]
fileList =
  [ File "2026-01-15" (Just "alice-to-tribunal.pdf")
      "Alice's submissions to the Tribunal"
  , File "2025-12-23" Nothing
      "Bob's submissions to the Tribunal (confidential)"
  , File "2025-12-17" (Just "tribunal-directions.pdf")
      "Orders made at the directions hearing"
  ]
```

I want Hakyll to generate an HTML page that formats this list as a
`<table>`.  Importantly, I also want Hakyll to notice when
`Files.hs` changes and update the output, *even when nothing else
changed*.


## Context and template

We first define how to process a `File` into a `Context File` that
can be fed to a template.

```haskell
fileContext :: Context File
fileContext =
  field "filename"
    ( maybe
        (noResult "unavailable")
        (pure . toUrl . ("files/" <>))
    . fileName . itemBody )
  <> field "date" (pure . fileDate . itemBody)
  <> field "desc" (pure . fileDesc . itemBody)
```

The Hakyll compiler for the HTML page populates a `listField` with
contents of `fileList`.  `fileContext` generates the context for the
individual items.

```haskell
import Hakyll
import Files

main :: IO ()
main = hakyll $ do

  create ["files.html"] $ do
    route idRoute
    compile $ do
      let filesContext =
            listField "files" fileContext
              (traverse makeItem fileList)

      makeItem ""
        >>= loadAndApplyTemplate
              "templates/files.html" filesContext
        >>= loadAndApplyTemplate
              "templates/default.html" defaultContext
        >>= relativizeUrls
```

The template content follows:

```html
<table id="files">
    <tr><th colspan="3">Tribunal proceeding files</th></tr>
    $for(files)$
    <tr>
        <td>$date$</td>
        $if(filename)$
        <td><a href="$filename$">download</a></td>
        $else$
        <td><em>N/A</em></td>
        $endif$
        <td>$desc$</td>
    </tr>
    $endfor$
</table>
```

The `noResult` in the `Nothing` case for the `fileName` field is
what makes the `$if(filename)$` / `$else$` conditional work.  Apart
from that, the template is trivial.


## Recompiling on change

One more thing.  Because the source of the data is not a markdown
(or whatever) file, Hakyll won't automatically notice if the data
changed and regenerate the HTML.  We have to set up an explicit
dependency on the Haskell source file.

Add a `match` rule for the relevant `.hs` file, so that Hakyll will
monitor it.  Also update the compiler for `files.html` load the
`Files.hs` "item", to establish the dependency:

```haskell
main = hakyll $ do

  match "Files.hs" $ do
    compile $ makeItem ()

  create ["files.html"] $ do
    route idRoute
    compile $ do
      _ <- load "Files.hs" :: Compiler (Item ())
      let filesContext =
        …
```

And that's all there is to it!
