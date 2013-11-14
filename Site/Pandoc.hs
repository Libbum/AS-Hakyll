{-# LANGUAGE OverloadedStrings #-}

module Site.Pandoc (pandocHtml5Compiler) where

import Hakyll.Web.Pandoc
import Text.Pandoc.Options (writerHtml5)

writerOpts = defaultHakyllWriterOptions { writerHtml5 = True }
pandocHtml5Compiler = pandocCompilerWith defaultHakyllReaderOptions writerOpts
