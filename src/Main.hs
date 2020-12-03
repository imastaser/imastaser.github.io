-- A significant part of this code has been borrowed from other
-- hakyll users, mostly Jasper through his site and hakyll's,
-- but also skybluetrades.net and chromaticleaves.com

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative             ((<$>))
import           Data.Char
import           Data.Maybe                      (catMaybes)
import           Data.Monoid                     (mappend, mconcat, mempty,
                                                  (<>))
import           Data.Time.Calendar              (toGregorian)
import           Data.Time.Clock                 (UTCTime (..), getCurrentTime)
import           Data.Time.Format                (formatTime)
import           GHC.IO.Encoding                 (setFileSystemEncoding,
                                                  setForeignEncoding,
                                                  setLocaleEncoding, utf8)
import           Hakyll
import           Hakyll.Web.Tags
import           System.FilePath.Posix           (addExtension, dropExtension,
                                                  replaceExtension,
                                                  splitDirectories,
                                                  takeBaseName, (</>))
import           System.Locale                   (defaultTimeLocale)
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Pandoc.Options


import           Compilers

config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync -avz -e ssh ./_site/ \
                       \ a@a.com:public_html/"
    , previewHost = "0.0.0.0"
    , previewPort = 8000
    }

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
                        .||. "posts/*/*.org"
                        .||. "posts/*/*/*/*.org"
                        .||. "posts/*/*/*/*/*.org"
                        .||. "posts/*/*/*/*/*/*.org")

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
           -- let title = "Posts tagged " ++ tag
            let title = tag
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
                posts <- fmap (take 12) . recentFirst =<< loadAll postPattern --  ((++) <$> loadAll "posts/*" <*> loadAll "posts/*/*")

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
                methodPosts <- recentFirst =<< loadAll ("posts/mitq/methodology/*"
                                                        .||. "posts/mitq/methodology/*/*")
                mathPosts <- recentFirst =<< loadAll ("posts/mitq/math/*"
                                                      .||. "posts/mitq/math/seminar/*"
                                                      .||. "posts/mitq/math/seminar/*/*"
                                                      .||. "posts/mitq/math/logic/*"
                                                      .||. "posts/mitq/math/logic/*/*"
                                                      .||. "posts/mitq/math/fvs/*"
                                                      .||. "posts/mitq/math/fvs/*/*"
                                                     )

                programmingPosts <- recentFirst =<< loadAll ("posts/mitq/programming/*"
                                                             .||. "posts/mitq/programming/*/*")
                haskellPosts <- recentFirst =<< loadAll ( "posts/mitq/haskell/*"
                                                        .||. "posts/mitq/haskell/*/*"
                                                        )
                logicPosts <- recentFirst =<< loadAll ("posts/mitq/logic/*" .||. "posts/mitq/logic/*/*" )
                etcPosts <- recentFirst =<< loadAll ("posts/mitq/etc/*" .||. "posts/mitq/etc/*/*" )
                fpPosts <- recentFirst =<< loadAll ("posts/mitq/fp/*" .||. "posts/mitq/fp/*/*")
                restPosts <- recentFirst =<< loadAll ("posts/mitq/*.org")

                let indexCtx =
                        listField "methodologyPosts" (postCtx tags) (if null methodPosts then fail "No posts" else return methodPosts) `mappend`
                        listField "programmingPosts" (postCtx tags) (if null programmingPosts then fail "No posts" else return programmingPosts) `mappend`
                        listField "haskellPosts" (postCtx tags) (if null haskellPosts then fail "No posts" else return haskellPosts) `mappend`
                        listField "logicPosts" (postCtx tags) (if null logicPosts then fail "No posts" else return logicPosts) `mappend`
                        listField "mathPosts" (postCtx tags) (if null mathPosts then fail "No posts" else return mathPosts) `mappend`
                        listField "fpPosts" (postCtx tags) (if null fpPosts then fail "No posts" else return fpPosts) `mappend`
                        listField "etcPosts" (postCtx tags) (if null etcPosts then fail "No posts" else return etcPosts) `mappend`
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
                astuacashuncPosts <- recentFirst =<< loadAll ("posts/hogi/astuatsashunch/*.markdown"
                                                              .||. "posts/hogi/astuatsashunch/*/*.markdown")
                mashalyanPosts <- recentFirst =<< loadAll ("posts/hogi/mashalyan/*.markdown"
                                                           .||. "posts/hogi/mashalyan/*/*.markdown")
                narekPosts <- recentFirst =<< loadAll ("posts/hogi/narek/*.markdown")
                nshnorhaliPosts <- recentFirst =<< loadAll ("posts/hogi/nshnorhali/*.markdown")
                grabarPosts <- recentFirst =<< loadAll ("posts/hogi/grabar/*.markdown")
                tatevaciPosts <- recentFirst =<< loadAll ("posts/hogi/tatevaci/*.markdown")
                restPosts <- recentFirst =<< loadAll ("posts/hogi/*.markdown")
                let indexCtx =
                        listField "astuacashuncPosts" (postCtx tags) (if null astuacashuncPosts then fail "No posts" else return astuacashuncPosts) `mappend`
                        listField "mashalyanPosts" (postCtx tags) (if null mashalyanPosts then fail "No posts" else return mashalyanPosts) `mappend`
                        listField "narekPosts" (postCtx tags) (if null narekPosts then fail "No posts" else return narekPosts) `mappend`
                        listField "nshnorhaliPosts" (postCtx tags) (if null nshnorhaliPosts then fail "No posts" else return nshnorhaliPosts) `mappend`
                        listField "grabarPosts" (postCtx tags) (if null grabarPosts then fail "No posts" else return grabarPosts) `mappend`
                        listField "tatevaciPosts" (postCtx tags) (if null tatevaciPosts then fail "No posts" else return tatevaciPosts) `mappend`
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
    , feedAuthorEmail = "arthur@imast.am"
    , feedRoot        = "http://imast.am"
    }


-- -----------------------------------------------------------------------------
-- * Helpers
--
getCurrentYear :: IO String
getCurrentYear = do
  -- getCurrentTime >>= return . toGregorian . utctDay
  now <- getCurrentTime
  let (year, _, _) = toGregorian $ utctDay now
  return $ show year



myTagsField :: String -> Tags -> Context a
myTagsField =
    tagsFieldWith getTags renderOneTag $ \tagLinks -> do
        H.ul ! A.class_ "list-inline" $ do
            H.li $ H.i ! A.class_ "fa fa-tags" $ mempty
            -- H.li . H.i $ mempty
            sequence_ tagLinks


renderOneTag :: String -> Maybe FilePath -> Maybe H.Html
renderOneTag _ Nothing           = Nothing
renderOneTag tag (Just filepath) =
    Just $ H.li $
        H.a ! A.href (toValue $ toUrl filepath) $ toHtml tag

--------------------------------------------------------------------------------



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

-- Megaparsec
{-
org :: Parser (Title, Date)
org = (,) <$> title <*> date <* takeRest

title :: Parser Title
title = Title . pack <$> (string "#+TITLE: " *> someTill anyChar newline)

date :: Parser Date
date = do
  string "#+DATE: "
  year  <- L.decimal <* char '-'
  month <- fmap (toEnum . pred) L.decimal <* char '-'
  day   <- L.decimal
  pure $ Date year month day

parseOrg :: Text -> Text -> Either Text (Title, Date)
parseOrg fp t = first (pack . parseErrorPretty) $ parse org (unpack fp) t
-}
