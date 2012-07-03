{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))

import Hakyll
import Text.Pandoc

main :: IO ()
main = hakyll $ do
	
	-- Stylesheets.
	match "css/*.css" $ do
		route idRoute
		compile compressCssCompiler
	match "css/start/*.css" $ do
		route idRoute
		compile compressCssCompiler

	-- Stylesheet images.
	match "css/start/images/*" $ do
		route idRoute
		compile copyFileCompiler
	match "css/*.gif" $ do
		route idRoute
		compile copyFileCompiler
 
	-- Images.
	match "images/*" $ do	
		route idRoute
		compile copyFileCompiler
	match "images/graphs/*.png*" $ do
		route idRoute
		compile copyFileCompiler

	-- Javascript files.
	match "js/*" $ do
		route idRoute
		compile copyFileCompiler

	-- Favicon.
	match "favicon.ico" $ do
		route idRoute
		compile copyFileCompiler

	-- Templates.
	match "templates/*" $ do
		compile templateCompiler

	-- Pages.
	match (list ["index.md", "benchmarks.md", "people.md", "publications.md"]) $ do
		route $ setExtension "html"
		compile $ pageCompiler
			>>> applyTemplateCompiler "templates/default.html"
			>>> relativizeUrlsCompiler
 
    -- How-to page (w/ TOC)
    match "howto.md" $ do
            route $ setExtension "html"
            compile $ pageCompilerWith defaultHakyllParserState withToc
                    >>> applyTemplateCompiler "templates/default.html"
                    >>> relativizeUrlsCompiler

	match "results.md" $ do
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/results.html"
            >>> relativizeUrlsCompiler
  where
    withToc = defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate = "$toc$\n$body$"
        , writerStandalone = True
        }
