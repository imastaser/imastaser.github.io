-- A significant part of this code has been borrowed from other
-- hakyll users, mostly Jasper through his site and hakyll's,
-- but also skybluetrades.net and chromaticleaves.com

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative ((<$>))
import Data.Char
import Data.Maybe (catMaybes)
import Data.Monoid (mappend, (<>), mconcat, mempty)
import Data.Time.Format (formatTime)
import Data.Time.Clock (getCurrentTime)
import Hakyll ((.||.), (.&&.), hakyllWith, match, idRoute, route,
               applyTemplateList, buildTags, compile,  compressCssCompiler,
               pandocCompilerWith, deployCommand, modificationTimeField,
               recentFirst, relativizeUrls, field, applyAsTemplate, listField,
               copyFileCompiler, dateField, defaultContext, toUrl, makeItem, loadAndApplyTemplate,
               defaultHakyllWriterOptions, getUnderlying, getMetadataField, defaultConfiguration,
               loadAll, loadBody, getResourceBody, create, setExtension, saveSnapshot,
               FeedConfiguration(..), fromCapture, loadAllSnapshots, composeRoutes, templateCompiler,
               constField, gsubRoute, defaultHakyllReaderOptions, bodyField, customRoute, toFilePath,
               Compiler, Context, Item, Pattern, Tags, Configuration, renderRss, renderAtom, Routes)

import Hakyll.Web.Tags
import System.Locale (defaultTimeLocale)
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import Text.Pandoc
import Text.Pandoc.Options
import GHC.IO.Encoding(utf8, setLocaleEncoding, setFileSystemEncoding, setForeignEncoding)
import System.FilePath.Posix  (takeBaseName, splitDirectories, (</>)
                             , addExtension, addExtension
                             , replaceExtension, dropExtension)

main :: IO ()
main = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8
    year <- getCurrentYear

    hakyllWith config $ do
    -- copy static assets ...
        let assets = ["favicon.ico", "images/*", "CNAME", "js/*",
                       "css/*.ttf"]

        let postPattern = ("posts/*"
                        .||. "posts/*/*/*"
                        .||. "posts/*/*.markdown"
                        .||. "posts/*/*/*/*.markdown")

        match (foldr1 (.||.) assets) $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*.css" $ do
            route   idRoute
            compile compressCssCompiler

        match "templates/*" $ compile templateCompiler

        -- build tags
        tags <- buildTags postPattern (fromCapture "tags/*.html")

        match postPattern $ do
            route  $ customRoute $  processPostsRoute .  toFilePath -- . niceRoute' -- "mitq/" --   $ setExtension "html"
            compile $ myPandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/default.html" (defaultContext `mappend` yearCtx year)
                >>= relativizeUrls

        match "pages/*" $ do
            route $ gsubRoute "pages/" (const "") `composeRoutes`
                setExtension ".html"
            compile $ myPandocCompiler
                  >>= loadAndApplyTemplate "templates/page.html" defaultContext
                  >>= loadAndApplyTemplate "templates/default.html" (defaultContext `mappend` yearCtx year)
                  >>= relativizeUrls

        create ["rss.xml"] $ do
            route idRoute
            compile $ do
                loadAllSnapshots postPattern "content"
                    >>= fmap (take 10) . recentFirst
                    >>= renderRss myFeedConfiguration feedCtx

        create ["atom.xml"] $ do
            route idRoute
            compile $ do
                loadAllSnapshots postPattern "content"
                    >>= fmap (take 10) . recentFirst
                    >>= renderAtom myFeedConfiguration feedCtx

        -- create a listing of all posts, most recent first
        create ["posts.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postPattern
                let ctx = constField "title" "Posts" <>
                          listField  "posts" (postCtx tags) (return posts ) <>
                          defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" (ctx `mappend` yearCtx year)
                    >>= relativizeUrls

        -- Post tags
        tagsRules tags $ \tag pattern -> do
            let title = "Posts tagged " ++ tag

            -- Copied from posts, need to refactor
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll pattern
                let ctx = constField "title" title <>
                            listField "posts" (postCtx tags) (return posts) <>
                            defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" (ctx `mappend` yearCtx year)
                    >>= relativizeUrls

        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- fmap (take 5) . recentFirst =<< loadAll postPattern --  ((++) <$> loadAll "posts/*" <*> loadAll "posts/*/*")
                let indexCtx =
                      listField "posts" (postCtx tags) (return posts) <>
                      field "tagcloud" (\_ -> myTagCloud tags) <>
                      defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        -- Mitq list
        match ("posts/mitq/index.html") $ do
            route  $ customRoute $  (processPagesRoute "mitq") .  toFilePath
            compile $ do
                methodPosts <- recentFirst =<< loadAll ("posts/mitq/methodology/*" .||. "posts/mitq/methodology/*/*")
                programmingPosts <- recentFirst =<< loadAll ("posts/mitq/programming/*" .||. "posts/mitq/programming/*/*")
                haskellPosts <- recentFirst =<< loadAll ("posts/mitq/haskell/*" .||. "posts/mitq/haskell/*/*" )
                fpPosts <- recentFirst =<< loadAll ("posts/mitq/fp/*" .||. "posts/mitq/fp/*/*")
                restPosts <- recentFirst =<< loadAll ("posts/mitq/*.markdown")

                let indexCtx =
                        listField "methodologyPosts" (postCtx tags) (if null methodPosts then fail "No posts" else return methodPosts) `mappend`
                        listField "programmingPosts" (postCtx tags) (if null programmingPosts then fail "No posts" else return programmingPosts) `mappend`
                        listField "haskellPosts" (postCtx tags) (if null haskellPosts then fail "No posts" else return haskellPosts) `mappend`
                        listField "fpPosts" (postCtx tags) (if null fpPosts then fail "No posts" else return fpPosts) `mappend`
                        listField "restPosts" (postCtx tags) (if null restPosts then fail "No posts" else return restPosts) `mappend`
                        defaultContext
                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        -- hogi list
        match ("posts/hogi/index.html") $ do
            route  $ customRoute $  (processPagesRoute "hogi") .  toFilePath
            compile $ do
                mashalyanPosts <- recentFirst =<< loadAll ("posts/hogi/mashalyan/*.markdown" .||. "posts/hogi/mashalyan/*/*.markdown")
                restPosts <- recentFirst =<< loadAll ("posts/hogi/*.markdown")
                let indexCtx =
                        listField "mashalyanPosts" (postCtx tags) (if null mashalyanPosts then fail "No posts" else return mashalyanPosts) `mappend`
                        listField "restPosts" (postCtx tags) (if null restPosts then fail "No posts" else return restPosts) `mappend`
                        defaultContext
                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls


        -- marmin list
        match ("posts/marmin/index.html") $ do
            route  $ customRoute $  (processPagesRoute "marmin") .  toFilePath
            compile $ do
                marminPosts <- recentFirst =<< loadAll "posts/marmin/*/*"
                restPosts <- recentFirst =<< loadAll ("posts/marmin/*.markdown")
                let indexCtx =
                        listField "marminPosts" (postCtx tags) (return marminPosts) `mappend`
                        listField "restPosts" (postCtx tags) (return restPosts) `mappend`
                        defaultContext
                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

-- -----------------------------------------------------------------------------
-- * Contexts

-- | Creates a "year" context from a string representation of the current year
yearCtx :: String -> Context String
yearCtx year = field "year" $ \item -> return year

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , dateField "date" "%B %e, %Y"
    , defaultContext
    ]

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%e %b %Y"
    , dateField "date" "%B %e, %Y"
    , myTagsField "tags" tags
    , defaultContext
    ]

pageCtx :: Context String
pageCtx = mconcat
    [ modificationTimeField "mtime" "%e %b %Y"
    , defaultContext
    ]

-- -----------------------------------------------------------------------------
-- * Feed configuration

-- | Holds my feed's configuration
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Arthur Vardanyan's blog"
    , feedDescription = ""
    , feedAuthorName  = "Arthur Vardanyan"
    , feedAuthorEmail = "artie.vard@gmail.com"
    , feedRoot        = "http://imastaser.am"
    }

-- -----------------------------------------------------------------------------
-- * Compilers

-- | Creates a compiler to render a list of posts for a given pattern, context,
-- and sorting/filtering function
postList :: Pattern
         -> Context String
         -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList pattern postCtx sortFilter = do
    posts   <- sortFilter =<< loadAll pattern
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateList itemTpl postCtx posts

-- -----------------------------------------------------------------------------
-- * Helpers
--
getCurrentYear :: IO String
getCurrentYear = return "2016" -- formatTime defaultTimeLocale "%Y" <$> getCurrentTime

myTagCloud :: Tags -> Compiler String
myTagCloud tags =
    renderTagCloud 80 250 tags

myTagsField :: String -> Tags -> Context a
myTagsField =
    tagsFieldWith getTags renderOneTag $ \tagLinks -> do
        H.ul ! A.class_ "list-inline" $ do
            H.li $ H.i ! A.class_ "fa fa-tags" $ mempty
            sequence_ tagLinks


renderOneTag :: String -> Maybe FilePath -> Maybe H.Html
renderOneTag _ Nothing           = Nothing
renderOneTag tag (Just filepath) =
    Just $ H.li $
        H.a ! A.href (toValue $ toUrl filepath) $ toHtml tag

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync -avz -e ssh ./_site/ \
                       \ alp@alpmestan.com:public_html/"
    }


myPandocCompiler' :: Maybe String -> Compiler (Item String)
myPandocCompiler' withToc =
    pandocCompilerWith defaultHakyllReaderOptions $
        case withToc of
            Just x | map toLower x `elem` ["true", "yes"] -> writerWithToc
                   | otherwise                            -> writerOpts
            Nothing                                       -> writerOpts

    where writerOpts = defaultHakyllWriterOptions
                           { writerReferenceLinks = True
                           , writerSectionDivs = True
                           , writerHtml5 = True
                           , writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
                           , writerColumns = 100
                           }
          writerWithToc =
            writerOpts { writerTableOfContents = True
                       , writerTemplate = "$if(toc)$<div id=\"toc\"><h3>Table of contents</h3>$toc$</div>$endif$\n$body$"
                       , writerStandalone = True
                       }

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = do
    ident <- getUnderlying
    myPandocCompiler' =<< getMetadataField ident "toc"

-- ------------------------------------------------------------------------------
-- * routes
--
--
-- instance IsString FilePath where
--   definitions

processPostsRoute :: [Char] -> FilePath
processPostsRoute = processRoute 1 ""

processPagesRoute :: FilePath -> [Char] -> FilePath
processPagesRoute = processRoute 2

processRoute :: Int -> FilePath -> [Char] -> FilePath
processRoute n root path =  root </> (foldl (</>) (fp "") rs) </>  (replaceExtension fileName "html")
                                        where fileName =  last ps
                                              rs   | length ps < 2  = [fp ""]
                                                   | otherwise = drop n $ init ps

                                              ps = splitDirectories path

                                              fp :: FilePath -> FilePath
                                              fp a = a





niceRoute :: String -> Routes
niceRoute prefix = customRoute $ \ident -> prefix ++ (takeBaseName . toFilePath $ ident) ++ "/index.html"

-- niceRoute' :: Routes
-- niceRoute' =  \ident -> (takeBaseName . toFilePath $ ident) ++ "/index.html"
