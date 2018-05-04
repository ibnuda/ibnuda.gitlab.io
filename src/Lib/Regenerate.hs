{-# LANGUAGE RecordWildCards #-}
module Lib.Regenerate where

import           Lib.Prelude                     hiding (div, head)

import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as TL
import           Data.Time
import           Data.Time.Clock.POSIX           (posixSecondsToUTCTime)
import qualified Text.Blaze.Html.Renderer.Pretty as BP
import           Text.Blaze.Html5                as H hiding (map)
import           Text.Blaze.Html5.Attributes     as A
import           Text.Markdown

import           Lib.ReadWrite
import           Lib.Types

generateSite config@Configuration {..} = do
  contentpages <- readDirectory pathPages
  contentposts <- readDirectory pathPosts
  putText ""

generateSideMenu :: [(Title, UTCTime)] -> Html
generateSideMenu titledates = do
  div ! id "menu" $ do
    div ! class_ "pure-menu" $ do
      a ! class_ "pure-menu-heading" ! href "index.html" $ text "Index"
      ul ! class_ "pure-menu-list" $ forM_ titledates generateSideMenuItem

generateSideMenuItem :: (Text, UTCTime) -> Html
generateSideMenuItem (tit, date) = do
  li ! class_ "pure-menu-item" $ do
    a ! class_ "pure-menu-link" !
      href (textValue $ htmlFilenameFromTitleAndDate tit date) $
      text tit

rawpostContentToHtml :: Text -> Html
rawpostContentToHtml = toHtml . markdown def . TL.fromStrict
