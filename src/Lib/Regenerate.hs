{-# LANGUAGE RecordWildCards #-}
module Lib.Regenerate where

import           Protolude                     hiding (div, head)

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

generateFeed :: SiteInfo -> IO ()
generateFeed SiteInfo {..} = do
  now  <- getCurrentTime
  raws <- getRawposts siteinfoFiles
  let
    rawposts = reverse . sort $ filter (\x -> rawpostType x == Post) raws
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
  writeGenerated siteinfoPublic siteinfoCSS
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
    Link
      .   fromJust
      .   parseURI
      $   T.unpack siteinfoUrl
      </> htmlFilename (T.unpack rawpostTitle) rawpostDate
  makeAuthor = Author . T.unpack

generateSite :: SiteInfo -> IO ()
generateSite config@SiteInfo {..} = do
  rawposts <- getRawposts siteinfoFiles
  css      <- readFile siteinfoCSS
  let (posts, pages) = partition (\x -> rawpostType x == Post) rawposts
  let indexcontent = generateIndex posts
      mymenu       = generateBottomLinks pages siteinfoRSS
  forM_ rawposts $ generateSingleHtml config
  writeGenerated siteinfoPublic "index.html" $ render $ layout config
                                                               css
                                                               mymenu
                                                               "Index"
                                                               indexcontent

render :: Html -> Text
render = TL.toStrict . BT.renderHtml

titdateToAttrVal :: Text -> UTCTime -> AttributeValue
titdateToAttrVal t d =
  let titlestr = T.unpack t
  in  textValue . T.pack . htmlFilename titlestr $ d

generateIndex :: [Rawpost] -> Html
generateIndex posts = div ! class_ "isi" $ do
  hr
  ul $ forM_ (reverse . sort $ posts) $ \Rawpost {..} ->
    generateIndexItem rawpostDate rawpostTitle
 where
  generateIndexItem d tit = li $ a ! href (titdateToAttrVal tit d) $ do
    text (formatteddate d <> " - " <> tit)
  formatteddate d = T.pack $ formatTime defaultTimeLocale "%z%F" $ utctDay d

generateSingleHtml :: SiteInfo -> Rawpost -> IO ()
generateSingleHtml config@SiteInfo {..} Rawpost {..} = do
  rawposts <- getRawposts siteinfoFiles
  css      <- readFile siteinfoCSS
  let pages           = filter (\(Rawpost _ _ _ d _) -> d == Page) rawposts
      mymenu          = generateBottomLinks pages siteinfoRSS
      filename        = htmlFilename (T.unpack rawpostTitle) rawpostDate
      renderedcontent = render $ layout
        config
        css
        mymenu
        rawpostTitle
        (contentMarkdownToHtml rawpostContent)
  writeGenerated siteinfoPublic filename renderedcontent

contentMarkdownToHtml :: Text -> Html
contentMarkdownToHtml md = div ! class_ "isi" $ do
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
      forM_ titledates $ \(t, d) -> do
        div ! class_ "isian" $ do
          b $ a ! href (titdateToAttrVal t d) $ text t
      div ! class_ "isian" $ do
        b $ a ! href (textValue $ T.pack feed) $ text "Feed"
    hr
    b $ do
      text "This material is shared under the "
      a ! href "https://creativecommons.org/licenses/by/4.0" $ text
        "CC-BY License."

layout :: SiteInfo -> Text -> Html -> Title -> Html -> Html
layout SiteInfo {..} css mymenu titlecontent htmlcontent =
  docTypeHtml ! lang "en" $ do
    head $ do
      H.meta ! charset "utf-8"
      H.meta ! name "viewport" ! content
        "width=device-width, initial-scale=1.0, user-scalable=yes"
      H.link ! rel "manifest" ! href "manifest.json"
      H.meta ! name "author" ! value (textValue siteinfoAuthor)
      H.meta ! name "theme-color" ! content "#2A2B2A"
      H.meta ! name "name" ! content "Nothing Unusual"
      H.meta ! name "short_name" ! content "Normal"
      H.meta ! name "description" ! content
        (textValue $ T.take 100 titlecontent)
      H.style $ text css
      H.link ! rel "icon" ! href "favicon.ico"
      H.title $ text $ siteinfoName <> " - " <> titlecontent
    body $ do
      div ! class_ "layout" $ do
        div ! class_ "atas" $ h1 $ text titlecontent
        htmlcontent
        mymenu
