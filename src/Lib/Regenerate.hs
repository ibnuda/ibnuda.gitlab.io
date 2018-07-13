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
  now  <- getCurrentTime
  raws <- getRawposts siteinfoFiles
  let rawposts = reverse . sort $ filter (\x -> rawpostType x == Post) raws
  let
    rssTitle = T.unpack siteinfoName
    rssLink  = fromJust . parseURI . T.unpack $ siteinfoUrl
    rssDesc  = "Nothing Unusual."
    rssElem =
      [ Language "en-us"
      , ManagingEditor (T.unpack siteinfoAuthor)
      , WebMaster (T.unpack siteinfoAuthor)
      , ChannelPubDate now
      , Generator
        "https://gitlab.com/ibnuda/ibnuda.gitlab.io/blob/master/src/Lib/RSS.hs"
      ]
    rssItem = map makeItems rawposts
  writeGenerated siteinfoPublic "feed.xml"
    . T.pack
    . showXML
    . rssToXML
    $ RSS {..}
 where
  makeItems rawp@Rawpost {..}
    = [ makeTitle rawpostTitle
      , makeLink rawp
      , makeAuthor siteinfoAuthor
      , PubDate rawpostDate
      , Description
        ( TL.unpack . BT.renderHtml . contentMarkdownToHtml $ T.take
          1000
          rawpostContent
        )
      ]
  makeTitle = Title . T.unpack
  makeLink Rawpost {..} =
    Link . fromJust . parseURI $ T.unpack siteinfoUrl </> generateHtmlFilename
      (T.unpack rawpostTitle)
      (rawpostDate)
  makeAuthor = Author . T.unpack

generateSite :: SiteInfo -> IO ()
generateSite config@SiteInfo {..} = do
  rawposts <- getRawposts siteinfoFiles
  let (posts, pages) = partition (\x -> rawpostType x == Post) rawposts
  let indexcontent = generateIndex posts
      mymenu       = generateBottomLinks pages siteinfoRSS
  forM_          rawposts       (generateSingleHtml config)
  writeGenerated siteinfoPublic "index.html" $ render $ layout config
                                                               mymenu
                                                               "Index"
                                                               indexcontent

render :: Html -> Text
render = TL.toStrict . BT.renderHtml

generateIndex :: [Rawpost] -> Html
generateIndex posts = do
  div ! class_ "isi" $ do
    hr
    ul $ forM_ (reverse . sort $ posts) $ \Rawpost {..} ->
      generateIndexItem rawpostDate rawpostTitle
 where
  generateIndexItem d tit = li $ do
    a
      ! href (textValue . T.pack . generateHtmlFilename (T.unpack tit) $ d)
      $ text (formatteddate d <> " - " <> tit)
  formatteddate d = T.pack $ formatTime defaultTimeLocale "%z%F" $ utctDay d

generateSingleHtml :: SiteInfo -> Rawpost -> IO ()
generateSingleHtml config@SiteInfo {..} Rawpost {..} = do
  rawposts <- getRawposts siteinfoFiles
  let pages           = filter (\(Rawpost _ _ _ d _) -> d == Page) rawposts
      mymenu          = generateBottomLinks pages siteinfoRSS
      renderedcontent = render $ layout
        config
        mymenu
        rawpostTitle
        (contentMarkdownToHtml rawpostContent)
      filename = generateHtmlFilename (T.unpack rawpostTitle) rawpostDate
  writeGenerated siteinfoPublic filename renderedcontent

contentMarkdownToHtml :: Text -> Html
contentMarkdownToHtml md =
  div ! class_ "isi" $ do
    hr
    preEscapedText $ CM.commonmarkToHtml [CM.optSafe] md

generateBottomLinks :: [Rawpost] -> Filename -> Html
generateBottomLinks rawpages feed = do
  let titledates = map (\pst -> (rawpostTitle pst, rawpostDate pst)) rawpages
  div ! class_ "bawah" $ do
    hr
    div ! class_ "kontainer" $ do
      div ! class_ "isian" $ do
        b $ a ! href "index.html" $ text "Back"
      forM_ titledates $ \(t, utct) -> do
        div ! class_ "isian" $ do
          b $ do
            a ! href (textValue $ T.pack $ generateHtmlFilename (T.unpack t) utct) $ do text t
      div ! class_ "isian" $ do
        b $ a ! href (textValue $ T.pack feed) $ text "Feed"
    hr
    b $ do
      text "This material is shared under the "
      a ! href "https://creativecommons.org/licenses/by/4.0" $ text
        "CC-BY License."

layout :: SiteInfo -> Html -> Title -> Html -> Html
layout SiteInfo {..} mymenu titlecontent htmlcontent =
  docTypeHtml ! lang "en" $ do
    head $ do
      H.meta ! charset "utf-8"
      H.meta ! name "viewport" ! content
        "width=device-width, initial-scale=1.0, user-scalable=yes"
      H.meta ! name "author" ! value (textValue siteinfoAuthor)
      H.meta ! name "theme-color" ! content "#2A2B2A"
      H.meta ! name "short_name" ! content "Normal"
      H.meta ! name "description" ! content
        (textValue (T.take 100 titlecontent))
      H.style $ text css
      H.link ! rel "icon" ! href "favicon.ico"
      H.title $ text $ siteinfoName <> " - " <> titlecontent
    body $ do
      div ! class_ "layout" $ do
        div ! class_ "atas" $ do
          h1 $ text titlecontent
        htmlcontent
        mymenu
css :: Text
css =
  "html{font-family:sans-serif;background-color:#f4f4f4}h1{font-size:2em;margin:0.67em 0}"
    <> "small{font-size:80%}img{width:100%}code,kbd,pre{font-family:monospace;font-size:1em}"
    <> "pre > code{display:block;border-left:0.1em solid #656565;padding:1rem 1.5rem;"
    <> "background-color:#f9f9f9;color:#656565;overflow-y:auto;overflow-x:auto}body{color:#000}"
    <> ".layout{display:flex;flex-direction:column;min-height:100vh;height:100%}"
    <> ".atas{margin:0;color:#2A2B2A;text-align:center}.isi{width:100%;align-self:center;"
    <> "max-width:800px;line-height:1.6em}.bawah{margin:auto auto 0;width:100%;max-width:800px}"
    <> ".kontainer{display:flex;flex-wrap:wrap;align-content:center}.isian{flex-direction:column;width:25%}"
