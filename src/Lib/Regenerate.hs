{-# LANGUAGE RecordWildCards #-}
module Lib.Regenerate where

import           Lib.Prelude                   hiding (div, head)

import           Data.List                     (partition)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Data.Time
import qualified Text.Blaze.Html.Renderer.Text as BT
import           Text.Blaze.Html5              as H hiding (map)
import           Text.Blaze.Html5.Attributes   as A
import           Text.Markdown

import           Lib.Rewrite
import           Lib.Types

defaultSiteInfo :: SiteInfo
defaultSiteInfo =
  SiteInfo
    "https://siskam.link"
    "Nothing Unusual"
    "Ibnu D. Aji"
    "posts"
    "public"

generateSite :: SiteInfo -> IO ()
generateSite config@SiteInfo {..} = do
  filenames <- getFiles siteinfoFiles
  rawposts <- mapM (readRawpost siteinfoFiles) filenames
  let (posts, pages) = partition (\x -> rawpostType x == Post) rawposts
  let indexcontent = generateIndex posts
      mymenu = generateSideMenu pages
  forM_ rawposts (generateSingleHtml config)
  writeGenerated siteinfoPublic "index.html" $
    render $ layout config mymenu "Title" indexcontent

render :: Html -> Text
render = TL.toStrict . BT.renderHtml

generateIndex :: [Rawpost] -> Html
generateIndex posts =
  ul $
  forM_ (reverse . sort $ posts) $ \Rawpost {..} ->
    generateIndexItem rawpostDate rawpostTitle
  where
    generateIndexItem d tit =
      li $ do
        a ! href (textValue . T.pack . generateHtmlFilename (T.unpack tit) $ d) $
          text (formatteddate d <> " - " <> tit)
    formatteddate d = T.pack $ formatTime defaultTimeLocale "%z%F" $ utctDay d

generateSingleHtml :: SiteInfo -> Rawpost -> IO ()
generateSingleHtml config@SiteInfo {..} Rawpost {..} = do
  fns <- getFiles siteinfoFiles
  rawposts <- mapM (readRawpost siteinfoFiles) fns
  let pages = filter (\(Rawpost _ _ _ d _) -> d == Page) rawposts
      mymenu = generateSideMenu pages
      renderedcontent =
        render $
        layout
          config
          mymenu
          rawpostTitle
          (contentMarkdownToHtml rawpostContent)
      filename = generateHtmlFilename (T.unpack rawpostTitle) (rawpostDate)
  writeGenerated siteinfoPublic filename renderedcontent

contentMarkdownToHtml :: Text -> Html
contentMarkdownToHtml md =
  div ! class_ "content" $ markdown def . TL.fromStrict $ md

generateSideMenu :: [Rawpost] -> Html
generateSideMenu rawpages = do
  let titledates = map (\pst -> (rawpostTitle pst, rawpostDate pst)) rawpages
  div ! id "menu" $ do
    div ! class_ "pure-menu" $ do
      a ! class_ "pure-menu-heading" ! href "index.html" $ text "Index"
      ul ! class_ "pure-menu-list" $ forM_ titledates generateSideMenuItem

generateSideMenuItem :: (Text, UTCTime) -> Html
generateSideMenuItem (tit, date) = do
  li ! class_ "pure-menu-item" $ do
    a ! class_ "pure-menu-link" !
      href (textValue $ T.pack $ generateHtmlFilename (T.unpack tit) date) $
      text tit

layout :: SiteInfo -> Html -> Title -> Html -> Html
layout SiteInfo {..} mymenu titlecontent htmlcontent =
  docTypeHtml ! lang "en" $ do
    head $ do
      H.meta ! charset "utf-8"
      H.meta ! name "viewport" !
        content "width=device-width, initial-scale=1.0, user-scalable=yes"
      H.meta ! name "author" ! value (textValue siteinfoAuthor)
      H.meta ! name "theme-color" ! content "#333"
      H.meta ! name "short_name" ! content "{} Unusual"
      H.meta ! name "description" !
        content (textValue (T.take 100 titlecontent))
      H.link ! rel "stylesheet" ! type_ "text/css" !
        href "static/pure-min-side-menu.css"
      H.link ! rel "icon" ! href "favicon.ico"
      H.title $ text $ siteinfoName <> " - " <> titlecontent
    body $ do
      div ! id "layout" $ do
        a ! name "not-a-menu" ! href "#menu" ! id "menuLink" ! class_ "menu-link" $
          H.span $ text T.empty
        mymenu
        div ! class_ "header" $ do h1 $ text titlecontent
        htmlcontent
      H.script $ text js
    footer $ do
      text "This material is shared under the "
      a ! href "https://creativecommons.org/licenses/by/4.0" $
        text "CC-BY License."

js :: Text
js =
  "(function(l,t){var g=t.getElementById('layout'),i=t.getElementById('menu')," <>
  "n=t.getElementById('menuLink'),o=t.getElementById('main');function toggleClass(l,t)" <>
  "{var g=l.className.split(/\\s+/),i=g.length,n=0;for(;n<i;n+=1){if(g[n]===t){g.splice(n,1); " <>
  "break}}if(i===g.length){g.push(t)}l.className=g.join(' ')}function toggleAll(l){var t='active';" <>
  "l.preventDefault();toggleClass(g,t);toggleClass(i,t);toggleClass(n,t)}n.onclick=function(l)" <>
  "{toggleAll(l)};o.onclick=function(l){if(i.className.indexOf('active')!==-1){toggleAll(l)}}})" <>
  "(this,this.document);"
