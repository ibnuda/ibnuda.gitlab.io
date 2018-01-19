{-# LANGUAGE RecordWildCards #-}
module Lib.Generate
  ( fullFledgedHtmlGeneration
  , defaultConfig
  ) where

import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as TL
import           Data.Time
import           Data.Time.Clock.POSIX           (posixSecondsToUTCTime)
import qualified Text.Blaze.Html.Renderer.Pretty as BP
import           Text.Blaze.Html5                as H hiding (map)
import           Text.Blaze.Html5.Attributes     as A
import           Text.Markdown

import           Lib.Prelude                     hiding (div, head)
import           Lib.ReadWrite
import           Lib.Types


markdownToHtml :: Text -> Html
markdownToHtml = toHtml . markdown def . TL.fromStrict

navigationItem :: Content -> Html
navigationItem Content {..} = do
  let titleAndDate = htmlFilenameFromTitleAndDate mdTitle mdDate
  li ! class_ "navigation-item" $ do
    a ! href (textValue titleAndDate) ! class_ "navigation-link" $
      toHtml mdTitle

navigationItems :: [Content] -> Html
navigationItems pages = do
  ul ! class_ "navigation-list float-right" $ forM_ pages $ navigationItem

navigationBar :: [Content] -> Html
navigationBar pages = do
  nav ! class_ "navigation" $ do
    section ! class_ "container" $ do
      a ! class_ "navigation-title" ! href "index.html" $ text "Home"
      navigationItems pages

mainContent :: Text -> Html
mainContent mdContent = do
  section ! class_ "container" $ do markdownToHtml mdContent


skeleton :: Configuration -> Html -> Text -> Text -> Html
skeleton Configuration {..} navbar titleContent markdownContent =
  docTypeHtml $ do
    head $ do
      H.meta ! name "viewport" ! value "width=device-width, initial-scale=1.0, user-scalable=yes"
      H.meta ! name "description" !
        value (textValue $ T.take 200 markdownContent)
      H.meta ! name "author" ! value (textValue author)
      H.link ! rel "stylesheet" ! type_ "text/css" !
        href "static/css/milligram.min.css"
      H.title $ text $ siteName <> " - " <> titleContent
    body $ do
      main ! class_ "wrapper" $ do
        navbar
        mainContent markdownContent

createIndexItem :: Content -> Content
createIndexItem cont =
  cont
  { filename = htmlFilenameFromTitleAndDate (mdTitle cont) (mdDate cont)
  }

indexItem :: Text -> Text -> UTCTime -> Html
indexItem postlink posttitle postdate =
  let dat = formatTime defaultTimeLocale "%z%F" $ utctDay postdate
  in li $ do
       a ! href (textValue postlink) $ text $ T.pack dat <> " - " <> posttitle

indexGeneration :: [Content] -> Text -> Content
indexGeneration posts path =
  let indexpage =
        ul $
        forM_ (reverse . sort $ posts) $ \pst ->
          indexItem (filename pst) (mdTitle pst) (mdDate pst)
  in Content "index.html" "Index" (posixSecondsToUTCTime 0) path $
     T.pack . BP.renderHtml $ indexpage


fullFledgedHtmlGeneration :: Configuration -> IO ()
fullFledgedHtmlGeneration conf = do
  cpages <- readDirectory $ pathPages conf
  cposts <- readDirectory $ pathPosts conf
  let navbar = navigationBar cpages
      generatedpages = map (mdContentToHtml conf navbar) cpages
      generatedposts = map (mdContentToHtml conf navbar) cposts
      contentindex = map createIndexItem cposts
  mapM_ writeHtml generatedposts
  writeHtml $
    indexHtml conf navbar $ indexGeneration contentindex (pathGenerated conf)
  mapM_ writeHtml generatedpages
  where
    mdContentToHtml :: Configuration -> Html -> Content -> Content
    mdContentToHtml config navbar cont =
      cont
      { filename = htmlFilenameFromTitleAndDate (mdTitle cont) (mdDate cont)
      , contentText =
          T.pack . BP.renderHtml $
          skeleton config navbar (mdTitle cont) (contentText cont)
      , contentType = pathGenerated config
      }
    indexHtml :: Configuration -> Html -> Content -> Content
    indexHtml config navbar cont =
      cont
      { contentText =
          T.pack . BP.renderHtml $
          skeleton config navbar (mdTitle cont) (contentText cont)
      }

defaultConfig :: Configuration
defaultConfig =
  Configuration
    "https://ibnuda.gitlab.io"
    "Nothing Unusual"
    "Ibnu D. Aji"
    "pages"
    "posts"
    "public"
