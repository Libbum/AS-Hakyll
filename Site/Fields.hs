{-# LANGUAGE OverloadedStrings #-}

module Site.Fields (
  postCtx
) where

import Data.Monoid (mappend)
import Hakyll

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext



