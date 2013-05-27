{-# LANGUAGE OverloadedStrings #-}
module Sitemap
( createSitemap
) where
--------------------------------------------------------------------------------
import           Data.Monoid         (mappend)
import qualified Data.Map                       as M
import           Data.List
import           Data.String
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           System.Posix.Files
import           System.Locale
import           Hakyll
import           Hakyll.Core.Metadata
import           Hakyll.Core.Identifier
import           UrlFixer
--------------------------------------------------------------------------------

createSitemap :: Identifier -> Compiler [Item String] -> Rules ()
createSitemap id pages =
    create [id] $ do
        route idRoute
        compile $ do
            let urlCtx = field "urls" (\_ -> urlList pages) `mappend` defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" urlCtx

urlList :: Compiler [Item String] -> Compiler String
urlList compPosts = do
    posts <- compPosts
    applyUrlTemplateList posts

applyUrlTemplateList :: [Item String] -> Compiler String
applyUrlTemplateList posts = do
    items  <- mapM applyUrlTemplate posts
    return $ concat $ intersperse "" $ map itemBody items

applyUrlTemplate :: Item String -> Compiler (Item String)
applyUrlTemplate item = do
    tpl  <- urlTemplate item
    applyTemplate tpl urlCtx item

urlTemplate ::  Item a -> Compiler Template
urlTemplate item = do
    metadata  <- getMetadata . itemIdentifier $ item
    case (M.lookup "image" metadata) of
         Nothing -> loadBody "templates/sitemap_element.xml"
         Just _  -> loadBody "templates/sitemap_element_with_image.xml"

getFullUrl :: String -> (Item a) -> Compiler String
getFullUrl root item = do
    mbPath <- getRoute.itemIdentifier $ item
    let fullUrl = case mbPath of
         Nothing  -> ""
         Just url ->  removeHtmlSuffix . (root ++) . toUrl $ url
    return fullUrl

getImageFullUrl :: String -> (Item a) -> Compiler String
getImageFullUrl root item = do
    metadata <- getMetadata . itemIdentifier $ item
    return $ case (M.lookup "image" metadata) of
         Nothing -> ""
         Just url -> root ++ url

getModificationTime :: (Item a) -> Compiler String
getModificationTime item = do
    let filePath = toFilePath . itemIdentifier $ item
    fileStatus <- unsafeCompiler . getFileStatus $ filePath
    let modTime = posixSecondsToUTCTime . realToFrac . modificationTime $ fileStatus
    let str = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S+00:00" modTime
    return str

urlCtx :: Context String
urlCtx =
    (field "url" $ getFullUrl "http://pilotssh.com")
    `mappend` (field "image_url" $ getImageFullUrl "http://pilotssh.com")
    `mappend` (field "last_modified" getModificationTime)
    `mappend` dateField "date" "%B %e, %Y"
    `mappend` defaultContext
