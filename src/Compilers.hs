{-# LANGUAGE OverloadedStrings #-}


module Compilers where

import           Data.Char           (toLower)

import           Hakyll
import           Text.Pandoc.Options


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



myTagCloud :: Tags -> Compiler String
myTagCloud tags =  renderTagList tags  -- show tag posts count in parns e.g haskell(3)
--  renderTagCloud 80 250 tags -- tag posts count feed to tag font size


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
                          -- , writerHtml5 = True
                           , writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
                           , writerColumns = 100
                           }
          writerWithToc =
            writerOpts { writerTableOfContents = True
                       , writerTemplate = Just "$if(toc)$<div id=\"TOC\">$toc$</div>$endif$\n$body$"
                    --   , writerStandalone = True
                       }

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = do
    ident <- getUnderlying
    myPandocCompiler' =<< getMetadataField ident "toc"

