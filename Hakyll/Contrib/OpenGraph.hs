module Hakyll.Contrib.OpenGraph
  ( openGraphField
  ) where

{- |

Open Graph metadata, as described in <https://ogp.me/>.

Requires key @"root"@ in the context, containing the HTTP URL to the root
of the Hakyll site.

-}
import Hakyll

openGraphField :: String -> Context String -> Context String
openGraphField k ctx = functionField k $ \_args i -> do
  template <- openGraphTemplate
  itemBody <$> applyTemplate template ctx i

openGraphTemplate :: Compiler Template
openGraphTemplate = do
  makeItem openGraphTemplateString >>= compileTemplateItem

openGraphTemplateString :: String
openGraphTemplateString =
  "<meta property=\"og:type\" content=\"article\" />\
  \<meta property=\"og:url\" content=\"$root$$url$\" />\
  \<meta property=\"og:title\" content=\"$title$\" />\
  \$if(og-description)$\
  \<meta property=\"og:description\" content=\"$og-description$\" />\
  \$endif$\
  \$if(og-image)$\
  \<meta property=\"og:image\" content=\"$og-image$\" />\
  \$endif$\
  \"
