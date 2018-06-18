{-# LANGUAGE RecordWildCards #-}
module Lib.Regenerate where

import           Lib.Prelude                   hiding (div, head)

import qualified CMark                         as CM
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

import           Lib.Rewrite
import           Lib.RSS
import           Lib.Types

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
  div ! class_ "content" $ preEscapedText $ CM.commonmarkToHtml [CM.optSafe] md

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
      -- H.link ! rel "stylesheet" ! type_ "text/css" ! href "static/pure-min-side-menu.css"
      H.style $ text css
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

css :: Text
css =
  "html{font-family:sans-serif;background-color:#eee}body{margin:0}footer," <>
  "header,main,menu,nav,section,summary{display:block}b,strong{font-weight:700}" <>
  "h1{font-size:2em;margin:.67em 0}small{font-size:80%}img{border:0;width:100%}" <>
  "hr{box-sizing:content-box;height:0}code,kbd,pre{font-family:monospace;font-size:1em}" <>
  "pre>code{display:block;border-left:.1em solid #656565;padding:1rem 1.5rem;" <>
  "background-color:#f9f9f9;color:#656565;overflow-y:auto;overflow-x:auto}" <>
  ".pure-menu-list{list-style:none;margin:0;padding:0}.pure-menu-item{padding:0;" <>
  "text-transform:uppercase;margin:0;height:100%;display:block}.pure-menu-heading," <>
  ".pure-menu-link{display:block;text-decoration:none;white-space:nowrap}" <>
  ".pure-menu-heading{text-transform:uppercase;color:#565d64}.pure-menu-link" <>
  "{color:#777}footer{float:right;position:fixed;bottom:0;right:2rem;font-size:" <>
  "80%;overflow:hidden;color:#242424}body{color:#000}#layout,#menu,.menu-link" <>
  "{-webkit-transition:all .2s ease-out;-moz-transition:all .2s ease-out;" <>
  "-ms-transition:all .2s ease-out;-o-transition:all .2s ease-out;transition:" <>
  "all .2s ease-out}#layout{position:relative;left:0;padding-left:0}#layout.active" <>
  " #menu{left:150px;width:150px}#layout.active .menu-link{left:150px}" <>
  ".content{margin:0 auto;padding:0 2em;max-width:800px;margin-bottom:50px;" <>
  "line-height:1.6em}.header{margin:0;color:#333;text-align:center;padding:2.5em 2em 0}" <>
  ".header h1{margin:.2em 0;font-size:3em;font-weight:300}#menu{height:calc(100%)" <>
  ";margin-left:-150px;width:150px;position:fixed;top:0;left:0;bottom:0;z-index:1000" <>
  ";overflow-y:auto;-webkit-overflow-scrolling:touch;display:flex;background:#eee;" <>
  "border-right:1px #000;flex-flow:column}#menu a{color:#333;border:0;padding:.6em 0" <>
  " .6em .6em;display:flex}#menu .pure-menu{flex:0 1 auto}#menu .filler{flex:1 1 auto}" <>
  "#menu .feed{flex:0 1 auto;display:block;background:#c26522;color:#fff;font-size:110%;" <>
  "text-transform:uppercase}#menu .pure-menu,#menu .pure-menu ul{border:0;background:" <>
  "transparent}#menu .pure-menu ul,#menu .pure-menu .menu-item-divided{border-top:1px" <>
  " solid #333}#menu .pure-menu li a:hover,#menu .pure-menu li a:focus{background:#333;" <>
  "color:#f2f2f2}#menu .pure-menu-heading{background:#333;color:#f2f2f2}#menu" <>
  " .pure-menu-heading{font-size:110%;color:#fff;margin:0}.menu-link{position:" <>
  "fixed;display:block;top:0;left:0;background:#000;background:rgba(0,0,0,0.7);" <>
  "font-size:12px;z-index:10;width:2em;height:auto;padding:2.1em 1.6em}.menu-link:" <>
  "hover,.menu-link:focus{background:#000}.menu-link span{position:relative;display:" <>
  "block}.menu-link span,.menu-link span:before,.menu-link span:after{background-color" <>
  ":#fff;width:100%;height:.2em}.menu-link span:before,.menu-link span:after{position:" <>
  "absolute;margin-top:-0.6em;content:\" \"}.menu-link span:after{margin-top:.6em}"
