{-# LANGUAGE RecordWildCards #-}
module Lib.Regenerate where

import           Lib.Prelude                   hiding (div, head)

import           Data.List                     (partition)
import           Data.Maybe                    (fromJust)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Data.Time
import           Network.URI
import           System.FilePath.Posix
import qualified Text.Blaze.Html.Renderer.Text as BT
import           Text.Blaze.Html5              as H hiding (map)
import           Text.Blaze.Html5.Attributes   as A
import           Text.Markdown

import           Lib.Rewrite
import           Lib.RSS
import           Lib.Types

defaultSiteInfo :: SiteInfo
defaultSiteInfo =
  SiteInfo
    "https://siskam.link"
    "Nothing Unusual"
    "iaji@siskam.link (Ibnu D. Aji)"
    "posts"
    "public"
    "feed.xml"

getRawposts :: FilePath -> IO [Rawpost]
getRawposts filepath = getFiles filepath >>= mapM (readRawpost filepath)

generatePakan :: SiteInfo -> IO ()
generatePakan SiteInfo {..} = do
  now <- getCurrentTime
  raws <- getRawposts siteinfoFiles
  let rawposts = reverse . sort $ filter (\x -> rawpostType x == Post) raws
  let rssTitle = T.unpack siteinfoName
      rssLink = fromJust . parseURI . T.unpack $ siteinfoUrl
      rssDesc = "Nothing Unusual."
      rssElem =
        [ Language "en-us"
        , ManagingEditor (T.unpack siteinfoAuthor)
        , WebMaster (T.unpack siteinfoAuthor)
        , ChannelPubDate now
        , Generator
            "https://gitlab.com/ibnuda/ibnuda.gitlab.io/blob/master/src/Lib/RSS.hs"
        ]
      rssItem = map makeItems rawposts
  writeGenerated siteinfoPublic "feed.xml" . T.pack . showXML . rssToXML $
    RSS {..}
  where
    makeItems rawp@Rawpost {..} =
      [ makeTitle rawpostTitle
      , makeLink rawp
      , makeAuthor siteinfoAuthor
      , PubDate rawpostDate
      , Description
          (TL.unpack . BT.renderHtml . contentMarkdownToHtml $
           T.take 1000 rawpostContent)
      ]
    makeTitle = Title . T.unpack
    makeLink Rawpost {..} =
      Link . fromJust . parseURI $
      T.unpack siteinfoUrl </>
      generateHtmlFilename (T.unpack rawpostTitle) (rawpostDate)
    makeAuthor = Author . T.unpack

generateSite :: SiteInfo -> IO ()
generateSite config@SiteInfo {..} = do
  rawposts <- getRawposts siteinfoFiles
  let (posts, pages) = partition (\x -> rawpostType x == Post) rawposts
  let indexcontent = generateIndex posts
      mymenu = generateSideMenu pages siteinfoRSS
  forM_ rawposts (generateSingleHtml config)
  writeGenerated siteinfoPublic "index.html" $
    render $ layout config mymenu "Index" indexcontent

render :: Html -> Text
render = TL.toStrict . BT.renderHtml

generateIndex :: [Rawpost] -> Html
generateIndex posts =
  div ! class_ "content" $ do
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
  rawposts <- getRawposts siteinfoFiles
  let pages = filter (\(Rawpost _ _ _ d _) -> d == Page) rawposts
      mymenu = generateSideMenu pages siteinfoRSS
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

generateSideMenu :: [Rawpost] -> Filename -> Html
generateSideMenu rawpages feed = do
  let titledates = map (\pst -> (rawpostTitle pst, rawpostDate pst)) rawpages
  div ! id "menu" $ do
    div ! class_ "pure-menu" $ do
      a ! class_ "pure-menu-heading" ! href "index.html" $ text "Index"
      ul ! class_ "pure-menu-list" $ do
        forM_ titledates generateSideMenuItem
    div ! class_ "filler" $ H.span ""
    a ! class_ "feed" ! href (textValue $ T.pack feed) $ text "Feed"

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
        div ! id "main" $ do
          div ! class_ "header" $ do h1 $ text titlecontent
          htmlcontent
      H.script $ text js
    footer $ do
      text "This material is shared under the "
      a ! href "https://creativecommons.org/licenses/by/4.0" $
        text "CC-BY License."

js :: Text
js =
  "(function(window,document){var layout=document.getElementById('layout')," <>
  "menu=document.getElementById('menu'),menuLink=document.getElementById('menuLink')" <>
  ",content=document.getElementById('main');function toggleClass(element,className)" <>
  "{var classes=element.className.split(/\\s+/),length=classes.length,i=0;for(;i<length;i+=1)" <>
  "{if(classes[i]===className){classes.splice(i,1);break}}if(length===classes.length)" <>
  "{classes.push(className)}element.className=classes.join(' ')}function toggleAll(e)" <>
  "{var active='active';e.preventDefault();toggleClass(layout,active);toggleClass(menu,active);" <>
  "toggleClass(menuLink,active)}menuLink.onclick=function(e){toggleAll(e)};" <>
  "content.onclick=function(e){if(menu.className.indexOf('active')!==-1){toggleAll(e)}}}(this,this.document));"
