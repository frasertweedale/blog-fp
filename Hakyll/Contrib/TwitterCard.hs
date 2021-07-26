module Hakyll.Contrib.TwitterCard
  ( twitterCardField
  ) where

{- |

Twitter Card metadata, as described at
<https://developer.twitter.com/en/docs/twitter-for-websites/cards/guides/getting-started>.

This feature should be used alongside "Hakyll.Contrib.OpenGraph".

-}
import Hakyll

twitterCardField :: String -> Context String -> Context String
twitterCardField k ctx = functionField k $ \_args i -> do
  template <- twitterCardTemplate
  itemBody <$> applyTemplate template ctx i

twitterCardTemplate :: Compiler Template
twitterCardTemplate = do
  makeItem twitterCardTemplateString >>= compileTemplateItem

twitterCardTemplateString :: String
twitterCardTemplateString =
  "<meta name=\"twitter:card\" content=\"summary\" />\
  \$if(twitter-creator)$<meta property=\"twitter:creator\" content=\"$twitter-creator$\" />$endif$\
  \$if(twitter-site)$<meta property=\"twitter:site\" content=\"$twitter-site$\" />$endif$"
