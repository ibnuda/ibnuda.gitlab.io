{-# LANGUAGE RecordWildCards #-}
module Lib.Rewrite where

import           Data.Char
import qualified Data.Text        as T
import           Data.Time
import           System.Directory
import qualified Text.Read        as TR
import           Text.Regex       (mkRegex, subRegex)

import           Lib.Prelude      hiding (intercalate)
import           Lib.Types


readRawpost :: [Char] -> IO Rawpost
readRawpost nameoffile = do
  content <- readFile nameoffile
  case T.lines content of
    title:date:tipe:writing -> do
      return $
        Rawpost
          nameoffile
          title
          (TR.read $ T.unpack date)
          (TR.read $ T.unpack tipe)
          (T.unlines writing)
    _ -> panic "invalid file."

getFiles :: FilePath -> IO [FilePath]
getFiles path =
  withCurrentDirectory path $ getDirectoryContents "." >>= filterM doesFileExist

deleteFiles :: FilePath -> IO ()
deleteFiles path = getFiles path >>= mapM_ removeFile

generateMdFilename :: [Char] -> [Char]
generateMdFilename input =
  subRegex (mkRegex "[^a-zA-Z0-9_.]") input "-"

generateHtmlFilename :: [Char] -> UTCTime -> [Char]
generateHtmlFilename title date =
  let fn = generateMdFilename  title
      dt = formatTime defaultTimeLocale "%z%F" $ utctDay date
  in map toLower $ show dt ++ "-" ++ fn

writeRawpost :: FilePath -> Rawpost -> IO ()
writeRawpost siteinfoFiles Rawpost {..} =
  writeFile
    (siteinfoFiles ++ "/" ++ rawpostFilename)
    (T.unlines [rawpostTitle, show rawpostDate, show rawpostType, rawpostContent])

createMdFile :: SiteInfo -> [Char] -> Title -> IO ()
createMdFile SiteInfo {..} ctype title = do
  now <- getCurrentTime
  let fn = generateMdFilename (T.unpack title)
      ct = TR.read ctype :: PostType
  writeRawpost siteinfoFiles $ Rawpost fn title now ct "Write here."
