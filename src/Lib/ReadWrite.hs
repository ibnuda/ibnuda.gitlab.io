{-# LANGUAGE RecordWildCards #-}
module Lib.ReadWrite
  ( readDirectory
  , writeContent
  , writeHtml
  , createMarkdownFile
  , htmlFilenameFromTitleAndDate
  , deleteOnlyFiles
  ) where

import           Data.Text
import           Data.Text.IO     hiding (putStrLn)
import           Data.Time
import           System.Directory
import qualified Text.Read        as TR
import           Text.Regex       (mkRegex, subRegex)

import           Lib.Prelude      hiding (intercalate)
import           Lib.Types

readIndividualFile :: Text -> [Char] -> IO Content
readIndividualFile above nameOfFile = do
  content <- readFile fn
  case lines content of
    title:date:writing ->
      pure $
      Content (pack nameOfFile) title (TR.read $ unpack date) above $
      unlines writing
    _ -> pure emptyContent
  where
    fn = unpack above ++ "/" ++ nameOfFile

getOnlyFiles :: FilePath -> IO [FilePath]
getOnlyFiles path =
  withCurrentDirectory path $ getDirectoryContents "." >>= filterM doesFileExist

deleteOnlyFiles :: Text -> IO ()
deleteOnlyFiles path =
  withCurrentDirectory (unpack path) $
    getDirectoryContents "." >>= filterM doesFileExist >>= mapM_ removeFile

readDirectory :: Text -> IO [Content]
readDirectory path = do
  listOfFiles <- getOnlyFiles $ unpack path
  mapM (readIndividualFile path) listOfFiles

mdFilenameFromTitle :: Text -> Text
mdFilenameFromTitle inp =
  pack $ subRegex (mkRegex "[^a-zA-Z0-9_.]") (unpack inp) "-"

htmlFilenameFromTitleAndDate :: Text -> UTCTime -> Text
htmlFilenameFromTitleAndDate title date =
  let fn = mdFilenameFromTitle title
      dat = formatTime defaultTimeLocale "%z%F" $ utctDay date
  in toLower $ pack dat <> "-" <> fn <> ".html" :: Text

write :: Text -> Text -> Text -> IO ()
write ctype fname content =
  writeFile (unpack ctype ++ "/" ++ unpack fname) content

writeContent :: Content -> IO ()
writeContent Content {..} =
  write contentType filename $
    unlines [mdTitle, show mdDate, contentText]

writeHtml :: Content -> IO ()
writeHtml Content {..} = write contentType filename contentText

createMarkdownFile :: Text -> Text -> IO ()
createMarkdownFile cType cTitle = do
  now <- getCurrentTime
  let filename = mdFilenameFromTitle cTitle
      dir =
        case cType of
          "page" -> "pages"
          "post" -> "posts"
          _      -> "posts" :: Text
  writeContent $
    Content (filename <> ".md") cTitle now dir "# Write Here!!!\nPlease."
