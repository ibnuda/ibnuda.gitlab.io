module Main where

import           Data.Text (intercalate, pack)
import           Lib       hiding (intercalate)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "new":ctype:titles ->
      createMarkdownFile (pack ctype) (intercalate " " . map pack $ titles)
    ["compile"] -> do
      deleteOnlyFiles $ pathGenerated defaultConfig
      fullFledgedHtmlGeneration defaultConfig
    ["clean"] -> deleteOnlyFiles $ pathGenerated defaultConfig
    _ -> do
      putText "should be one of the following:"
      putText "new post title_of_post"
      putText "new page title_of_page"
      putText "compile"
      putText "clean"
