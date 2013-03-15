--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           System.Posix.Files
import           System.Locale
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith myConfiguration $ do
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "font/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fancybox/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Copy favicon, htaccess...
    match "data/*" $ do
        route   $ gsubRoute "data/" (const "")
        compile copyFileCompiler

    match "faq.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/faq.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "blog/*.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["blog/index.html"] $ do
        route $ idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (\_ -> postList recentFirst) `mappend`
                    constField "title" "Archives"              `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= removeAllHtmlPrefix

    create ["index.html"] $ do
        route idRoute
        compile $ do
            let indexCtx = constField "title" "SSH for iPhone with Pilot SSH" `mappend` defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "script/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/script.html"  postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls


    match "github-btn.html" $ do
        route   idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateCompiler

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            let urlCtx = field "urls" (\_ -> urlList) `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" urlCtx

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend` defaultContext

--------------------------------------------------------------------------------
postList :: ([Item String] -> [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter <$> loadAll "blog/*.markdown"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list

--------------------------------------------------------------------------------
removeAllHtmlPrefix :: Item String -> Compiler (Item String)
removeAllHtmlPrefix item = do
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r  -> fmap (withUrls removeHtmlPrefix) item

removeHtmlPrefix :: String -> String
removeHtmlPrefix x = case (reverse . take 5 . reverse) $ x of
                     ".html" -> reverse . snd . splitAt 5 . reverse $ x
                     _       -> x
-------------------------------------------------------------------------------
urlList ::  Compiler String
urlList = do
    posts   <- loadAll "blog/*.markdown"
    itemTpl <- loadBody "templates/sitemap_element.xml"
    list    <- applyTemplateList itemTpl urlCtx posts
    return list

getFullUrl :: String -> (Item a) -> Compiler String
getFullUrl root item = do
    mbPath <- getRoute.itemIdentifier $ item
    let fullUrl = case mbPath of
         Nothing  -> ""
         Just url ->  (root ++) . toUrl $ url
    return fullUrl

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
    `mappend` (field "last_modified" getModificationTime)
    `mappend` dateField "date" "%B %e, %Y"
    `mappend` defaultContext

-------------------------------------------------------------------------------

-- Custom configuration

myConfiguration :: Configuration
myConfiguration = defaultConfiguration {ignoreFile = ignoreFile'}
  where
    ignoreFile' x
        | x == "data/.htaccess" = False
        | otherwise        = ignoreFile defaultConfiguration x
