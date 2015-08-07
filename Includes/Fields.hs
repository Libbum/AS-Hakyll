{-# LANGUAGE OverloadedStrings #-}

module Includes.Fields (
  postCtx,
  mathCtx
) where

import Data.Monoid (mappend)
import Hakyll
import qualified Data.Map as M

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext


mathCtx :: Context a
mathCtx = field "katex" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ if "katex" `M.member` metadata
                  then "<link rel=\"stylesheet\" href=\"/css/katex.min.css\">\n\
                       \<script type=\"text/javascript\" src=\"/js/katex.min.js\"></script>\n\
                       \<script src=\"/js/auto-render.min.js\"></script>"
                  else ""
