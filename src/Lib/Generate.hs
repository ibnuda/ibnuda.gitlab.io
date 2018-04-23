{-# LANGUAGE RecordWildCards #-}
module Lib.Generate where

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
  li ! class_ "pure-menu-item" $ do
    a ! class_ "pure-menu-link" ! href (textValue titleAndDate) $ do
      toHtml mdTitle

navigationItems :: [Content] -> Html
navigationItems pages = do
  div ! id "menu" $ do
    div ! class_ "pure-menu" $ do
      a ! href "index.html" ! class_ "pure-menu-heading" $
        toHtml ("Index" :: Text)
      ul ! class_ "pure-menu-list" $ do
        forM_ pages navigationItem

mainContent :: Text -> Html
mainContent mdContent = do
  div ! class_ "content" $ do markdownToHtml mdContent

bagianTitle :: Text -> Html
bagianTitle titlecontent = do
  div ! class_ "header" $ do
    h1 $ toHtml titlecontent

skeleton :: Configuration -> Html -> Text -> Text -> Html
skeleton Configuration {..} mymenu titleContent markdownContent =
  docTypeHtml $ do
    head $ do
      H.meta ! charset "utf-8"
      H.meta ! name "viewport" ! value "width=device-width, initial-scale=1.0, user-scalable=yes"
      H.meta ! name "author" ! value (textValue author)
      H.link ! rel "stylesheet" ! type_ "text/css" ! href "static/pure-min.css"
      H.link ! rel "stylesheet" ! type_ "text/css" ! href "static/side-menu.css"
      H.title $ text $ siteName <> " - " <> titleContent
    body $ do
      div ! id "layout" $ do
        a ! href "#menu" ! id "menuLink" ! class_ "menu-link" $ H.span $ toHtml T.empty
        mymenu
        div ! id "main" $ do
          bagianTitle titleContent
          mainContent markdownContent
      H.script ! src "static/ui.js" $ ""
    footer $ do
      text "This material is shared under the "
      a ! href "https://creativecommons.org/licenses/by/4.0" $
        text "CC-BY License."

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
  let navbar = navigationItems cpages
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
