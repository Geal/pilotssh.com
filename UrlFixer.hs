module UrlFixer
( removeAllHtmlSuffix
, removeHtmlSuffix
) where
--------------------------------------------------------------------------------
import           Hakyll
import           Hakyll.Core.Metadata
--------------------------------------------------------------------------------

removeAllHtmlSuffix :: Item String -> Compiler (Item String)
removeAllHtmlSuffix item = do
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r  -> fmap (withUrls removeHtmlSuffix) item

removeHtmlSuffix :: String -> String
removeHtmlSuffix x = case (reverse . take 5 . reverse) $ x of
                     ".html" -> reverse . snd . splitAt 5 . reverse $ x
                     _       -> x
