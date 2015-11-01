--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Data.List.Split (splitOn)
import Data.Monoid (mappend)
import Hakyll
import Includes.Fields
import Includes.Pandoc

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do

    match ("images/*" .||. "favicon.ico" .||. "js/*" .||. "papers/*" .||. "css/fonts/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "about.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocHtml5Compiler (storeDirectory config)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" $ fromCapture "tags/*.html"
    --pages <- buildPaginate "posts/*"

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            compiled <- pandocHtml5Compiler (storeDirectory config)
            let pagesCtx =
                    tagsField "tags" tags `mappend` postCtx

            full <- loadAndApplyTemplate "templates/post.html" pagesCtx compiled
            index <- loadAndApplyTemplate "templates/post-index.html" postCtx compiled
            blurb <- loadAndApplyTemplate "templates/post-blurb.html" postCtx $ getBlurb compiled
            saveSnapshot "content" full
            saveSnapshot "index" index
            saveSnapshot "blurb" blurb
            loadAndApplyTemplate "templates/default.html" (mathCtx `mappend` postCtx) full
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

    create ["papers.html"] $ do
        route idRoute
        compile $ do
            --TODO: Generate papers metadata
            let papersCtx =
                    constField "title" "Papers"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/papers.html" papersCtx
                >>= loadAndApplyTemplate "templates/default.html" papersCtx
                >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let tagCtx = constField "title" ("Posts tagged with " ++ tag) `mappend` defaultContext

        route idRoute
        compile $ do
            postsTagged tags pattern recentFirst
                >>= makeItem
                >>= loadAndApplyTemplate "templates/tag.html" tagCtx
                >>= loadAndApplyTemplate "templates/default.html" tagCtx
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
            posts <- drop 3 . (take 8) <$> (recentFirst =<< loadAll "posts/*")
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    field "first" (const (itemBody <$> mostRecentPost)) `mappend`
                    field "second" (const (itemBody <$> (head <$> recentBlurbs))) `mappend`
                    field "third" (const (itemBody <$> (last <$> recentBlurbs))) `mappend`
                    tagCloudField "cloud" 100 300 tags `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (mathCtx `mappend` indexCtx)
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
        { deployCommand = "rsync -avz -e ssh ./_site/ Akasha:axiomatic/" }

mostRecentPost :: Compiler (Item String)
mostRecentPost = head <$> (recentFirst =<< loadAllSnapshots "posts/*" "index")

recentBlurbs :: Compiler [Item String]
recentBlurbs = tail . (take 3) <$> (recentFirst =<< loadAllSnapshots "posts/*" "blurb")

atomFeedConfig :: FeedConfiguration
atomFeedConfig = FeedConfiguration
    { feedTitle       = "Axiomatic Semantics"
    , feedDescription = "Brute force solutions with proofs by intimidation"
    , feedAuthorName  = "Tim DuBois"
    , feedAuthorEmail = "tim@neophilus.net"
    , feedRoot        = "http://axiomatic.neophilus.net"
    }

postsTagged :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postsTagged tags pattern sortFilter = do
    template <- loadBody "templates/post-item.html"
    posts <- sortFilter =<< loadAll pattern
    applyTemplateList template postCtx posts

getBlurb :: Item String -> Item String
getBlurb = fmap (unwords . takeWhile (/= "<!--BLURB-->") . splitOn " ")
