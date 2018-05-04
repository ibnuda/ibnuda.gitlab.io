{-# LANGUAGE RecordWildCards #-}
module Lib.Rewrite where

import           Data.Char
import qualified Data.Text             as T
import           Data.Time
import           System.Directory
import           System.FilePath.Posix
import qualified Text.Read             as TR
import           Text.Regex            (mkRegex, subRegex)

import           Lib.Prelude           hiding (intercalate)
import           Lib.Types


readRawpost :: FilePath -> Filename -> IO Rawpost
readRawpost path nameoffile = do
  content <- readFile $ path ++ "/" ++ nameoffile
  case T.lines content of
    title:date:tipe:writing -> do
      return $
        Rawpost
          nameoffile
          title
          (TR.read $ T.unpack date)
          (TR.read $ T.unpack tipe)
          (T.unlines writing)
    _ -> panic "Invalid file."

getFiles :: FilePath -> IO [FilePath]
getFiles path =
  withCurrentDirectory path $ getDirectoryContents "." >>= filterM doesFileExist

deleteFiles :: FilePath -> IO ()
deleteFiles path = getFiles path >>= mapM_ (removeFile . (path </>))

generateMdFilename :: [Char] -> [Char]
generateMdFilename input =
  subRegex (mkRegex "[^a-zA-Z0-9_.]") input "-"

generateHtmlFilename :: [Char] -> UTCTime -> [Char]
generateHtmlFilename title date =
  let fn = generateMdFilename  title
      dt = formatTime defaultTimeLocale "%z%F" $ utctDay date
  in map toLower $ dt ++ "-" ++ fn ++ ".html"

writeRawpost :: FilePath -> Rawpost -> IO ()
writeRawpost siteinfofiles Rawpost {..} =
  writeFile
    (siteinfofiles ++ "/" ++ rawpostFilename)
    (T.unlines [rawpostTitle, show rawpostDate, show rawpostType, rawpostContent])

writeGenerated :: FilePath -> Filename -> Text -> IO ()
writeGenerated siteinfopublic filename content =
  writeFile (siteinfopublic ++ "/" ++ filename) content

createMdFile :: SiteInfo -> [Char] -> Title -> IO ()
createMdFile SiteInfo {..} ctype title = do
  now <- getCurrentTime
  let fn = generateMdFilename (T.unpack title)
      ct = TR.read ctype :: PostType
  writeRawpost siteinfoFiles $ Rawpost (fn ++ ".md") title now ct "Write here."
