--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.Options (writerHtml5)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    let writerOpts = defaultHakyllWriterOptions { writerHtml5 = True }
    let pandocHtml5Compiler = pandocCompilerWith defaultHakyllReaderOptions writerOpts

    match ("images/*" .||. "favicon.ico" .||. "js/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocHtml5Compiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" $ fromCapture "tags/*.html"

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            compiled <- pandocHtml5Compiler
            full <- loadAndApplyTemplate "templates/post.html" (taggedPostCtx tags) compiled
            blurb <- loadAndApplyTemplate "templates/post-blurb.html" postCtx $ cutMore compiled
            saveSnapshot "content" full
            saveSnapshot "blurb" blurb
            loadAndApplyTemplate "templates/default.html" postCtx full
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

    tagsRules tags $ \tag pattern -> do
        let tagCtx = constField "title" ("Posts tagged as " ++ tag) `mappend` defaultContext

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
            posts <- (take 5) <$> (recentFirst =<< loadAll "posts/*")
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    field "first" (const (itemBody <$> mostRecentPost)) `mappend`
                    field "second" (const (itemBody <$> (head <$> tail <$> recentBlurbs))) `mappend`
                    field "third" (const (itemBody <$> (last <$> recentBlurbs))) `mappend`
                    tagCloudField "cloud" 100 300 tags `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

config :: Configuration
config = defaultConfiguration
        {   deployCommand = "rsync -avz -e ssh ./_site/ Neophilus:www/axiomatic/hakyll"}

---TODO: Drop the tags off of this, we don't want them on the front page
mostRecentPost :: Compiler (Item String)
mostRecentPost = head <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")

recentBlurbs :: Compiler [Item String]
recentBlurbs = (take 3) <$> (recentFirst =<< loadAllSnapshots "posts/*" "blurb")

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

taggedPostCtx :: Tags -> Context String
taggedPostCtx tags = tagsField "tags" tags `mappend` postCtx

cutMore :: Item String -> Item String
cutMore = fmap (unlines . takeWhile (/= "<!-- MORE -->") . lines)
