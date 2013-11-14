{-# LANGUAGE OverloadedStrings #-}

module Site.Fields (
  postCtx,
  mathCtx
) where

import Data.Monoid (mappend)
import Hakyll
import qualified Data.Map as M

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext


mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ if "mathjax" `M.member` metadata
                  then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
                  else ""
