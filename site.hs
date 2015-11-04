--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import Hakyll
import Includes.Fields
import Includes.Pandoc

import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.HashMap.Strict as HM
import System.FilePath (takeFileName)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Clock (UTCTime)
import Data.List (intercalate, sortBy)
import Control.Applicative (Alternative (..))
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

    allPosts <- getMatches "posts/*"
    let sortedPosts = sortIdentifiersByDate allPosts
        (prevPostHM, nextPostHM) = buildAdjacentPostsHashMap sortedPosts

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            compiled <- pandocHtml5Compiler (storeDirectory config)
            let pagesCtx =
                    field "nextPost" (lookupPostUrl nextPostHM) <>
                    field "prevPost" (lookupPostUrl prevPostHM) <>
                    tagsField "tags" tags <> postCtx

            full <- loadAndApplyTemplate "templates/post.html" pagesCtx compiled
            index <- loadAndApplyTemplate "templates/post-index.html" postCtx compiled
            blurb <- loadAndApplyTemplate "templates/post-blurb.html" postCtx $ getBlurb compiled
            saveSnapshot "content" full
            saveSnapshot "index" index
            saveSnapshot "blurb" blurb
            loadAndApplyTemplate "templates/default.html" (mathCtx <> postCtx) full
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
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
                    constField "title" "Papers"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/papers.html" papersCtx
                >>= loadAndApplyTemplate "templates/default.html" papersCtx
                >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let tagCtx = constField "title" ("Posts tagged with " ++ tag) <> defaultContext

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
            let feedCtx = postCtx <> bodyField "description"
            ---TODO: May need to build a feed snapshot, the article class wrappers etc may get in the way.
            posts <- (take 10) <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")
            renderAtom atomFeedConfig feedCtx posts

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- drop 3 . (take 8) <$> (recentFirst =<< loadAll "posts/*")
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Home"                <>
                    field "first" (const (itemBody <$> mostRecentPost)) <>
                    field "second" (const (itemBody <$> (head <$> recentBlurbs))) <>
                    field "third" (const (itemBody <$> (last <$> recentBlurbs))) <>
                    tagCloudField "cloud" 100 300 tags <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> indexCtx)
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
        { deployCommand = "rsync -v -rz --checksum --delete -e ssh _site/ Akasha:axiomatic/" }

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


sortIdentifiersByDate :: [Identifier] -> [Identifier]
sortIdentifiersByDate identifiers =
    sortBy byDate identifiers
    where
    byDate id1 id2 =
        let fn1 = takeFileName $ toFilePath id1
            fn2 = takeFileName $ toFilePath id2
            parseTime' fn = parseTimeM True defaultTimeLocale "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn
        in compare ((parseTime' fn1) :: Maybe UTCTime) ((parseTime' fn2) :: Maybe UTCTime)

type AdjPostHM = HM.HashMap Identifier Identifier


instance Hashable Identifier where
    hashWithSalt salt = hashWithSalt salt . show


buildAdjacentPostsHashMap :: [Identifier] -> (AdjPostHM, AdjPostHM)
buildAdjacentPostsHashMap posts =
    let buildHM :: [Identifier] -> [Identifier] -> AdjPostHM
        buildHM [] _ = HM.empty
        buildHM _ [] = HM.empty
        buildHM (k:ks) (v:vs) = HM.insert k v $ buildHM ks vs
    in (buildHM (tail posts) posts, buildHM posts (tail posts))


lookupPostUrl :: AdjPostHM -> Item String -> Compiler String
lookupPostUrl hm post =
    let ident = itemIdentifier post
        ident' = HM.lookup ident hm
    in
    (fmap (maybe empty $ toUrl) . (maybe empty getRoute)) ident'


previousPostUrl :: [Identifier] -> Item String -> Compiler String
previousPostUrl sortedPosts post = do
    let ident = itemIdentifier post
        ident' = itemBefore sortedPosts ident
    (fmap (maybe empty $ toUrl) . (maybe empty getRoute)) ident'


nextPostUrl :: [Identifier] -> Item String -> Compiler String
nextPostUrl sortedPosts post = do
    let ident = itemIdentifier post
        ident' = itemAfter sortedPosts ident
    (fmap (maybe empty $ toUrl) . (maybe empty getRoute)) ident'


itemAfter :: Eq a => [a] -> a -> Maybe a
itemAfter xs x =
    lookup x $ zip xs (tail xs)


itemBefore :: Eq a => [a] -> a -> Maybe a
itemBefore xs x =
    lookup x $ zip (tail xs) xs


urlOfPost :: Item String -> Compiler String
urlOfPost =
    fmap (maybe empty $ toUrl) . getRoute . itemIdentifier

