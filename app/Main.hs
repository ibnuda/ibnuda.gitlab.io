module Main where

import           Data.Text (intercalate, pack)
import           Lib       hiding (intercalate)

main :: IO ()
main = do
  args <- getArgs
  siteinfo <- readSiteinfo
  case args of
    "new":ctype:titles ->
      createMdFile siteinfo ctype (intercalate " " . map pack $ titles)
    ["compile"] -> do
      deleteFiles (siteinfoPublic siteinfo)
      generateSite siteinfo
    ["clean"] -> deleteFiles (siteinfoPublic siteinfo)
    ["atom"] -> generatePakan siteinfo
    _ -> do
      putText "should be one of the following:"
      putText "new Post title_of_post"
      putText "new Page title_of_page"
      putText "compile"
      putText "clean"
      putText "atom"
