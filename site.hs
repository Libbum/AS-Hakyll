--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Data.Monoid (mappend)
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match ("images/*" .||. "favicon.ico" .||. "js/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            ---TODO: May need to build a feed snapshot, the article class wrappers etc may get in the way.
            posts <- (take 10) <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")
            renderAtom atomFeedConfig feedCtx posts

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    field "first" (const (itemBody <$> mostRecentPost)) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` metadataField `mappend` defaultContext

config :: Configuration
config = defaultConfiguration
        {   deployCommand = "rsync -avz -e ssh ./_site/ Neophilus:www/axiomatic/hakyll"}

mostRecentPost :: Compiler (Item String)
mostRecentPost = head <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")

atomFeedConfig :: FeedConfiguration
atomFeedConfig = FeedConfiguration
    { feedTitle       = "Axiomatic Semantics"
    , feedDescription = "Brute force solutions with proofs by intimidation"
    , feedAuthorName  = "Tim DuBois"
    , feedAuthorEmail = "tim@neophilus.net"
    , feedRoot        = "http://axiomatic.neophilus.net"
    }

