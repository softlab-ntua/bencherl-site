{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))

import Hakyll

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
	match (list ["index.md", "benchmarks.md", "download.md", "people.md", "publications.md"]) $ do
		route $ setExtension "html"
		compile $ pageCompiler
			>>> applyTemplateCompiler "templates/default.html"
			>>> relativizeUrlsCompiler
 
	match (list ["results.md"]) $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/results.html"
            >>> relativizeUrlsCompiler

