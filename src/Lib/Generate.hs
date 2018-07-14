{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Lib.Generate where

import           Protolude

import           Data.Yaml
import           Text.Mustache

import           Data.Maybe                    (fromJust)
import qualified CMark                         as CM
import           Data.List                     (partition)
import qualified Data.Text                     as T
import           Network.URI
import           Data.Time

import           Lib.Rewrite
import           Lib.RSS
import           Lib.Types

getRaws :: FilePath -> IO [Rawpost]
getRaws filepath = getFiles filepath >>= mapM (readRawpost filepath)

generateBlog :: SiteInfo -> IO ()
generateBlog SiteInfo {..} = do
  rawposts      <- getRaws siteinfoFiles
  maintemplate  <- compileMustacheDir "main" siteinfoTemplatesDir
  indextemplate <- compileMustacheDir "index" siteinfoTemplatesDir
  css           <- readFile siteinfoCSS
  let (posts, pages) = partition (\x -> rawpostType x == Post) rawposts
      links          = map mkMenuLink pages
      indexlinks     = map mkIndexLink $ reverse . sort $ posts
      index          = Index "Index" css "Index" links indexlinks
      indexpage      = render indextemplate index
  forM_ rawposts $ \p@Rawpost {..} -> do
    let htmlname = htmlFilename (T.unpack rawpostTitle) rawpostDate
        page     = render maintemplate $ mkPage css links p
    writeFile (siteinfoPublic ++ "/" ++ htmlname) page
  writeFile (siteinfoPublic ++ "/index.html") indexpage

render :: ToJSON a => Template -> a -> Text
render template a = toStrict $ renderMustache template $ toJSON a

mkPage :: Text -> [BlogLink] -> Rawpost -> BlogPage
mkPage css links Rawpost {..} =
  let desc    = T.take 100 rawpostTitle
      content = CM.commonmarkToHtml [] rawpostContent
  in  BlogPage rawpostTitle css desc content links

mkMenuLink :: Rawpost -> BlogLink
mkMenuLink Rawpost {..} =
  let htmlname = T.pack $ htmlFilename (T.unpack rawpostTitle) rawpostDate
  in  BlogLink htmlname rawpostTitle

mkIndexLink :: Rawpost -> BlogLink
mkIndexLink Rawpost {..} =
  let formatteddate =
        T.pack $ formatTime defaultTimeLocale "%z%F" $ utctDay rawpostDate
      title    = formatteddate <> " - " <> rawpostTitle
      htmlname = T.pack $ htmlFilename (T.unpack rawpostTitle) rawpostDate
  in  BlogLink htmlname title

generateFeed :: SiteInfo -> IO ()
generateFeed SiteInfo {..} = do
  now  <- getCurrentTime
  raws <- getRaws siteinfoFiles
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
  makeItems rawp@Rawpost {..} =
    [ makeTitle rawpostTitle
    , makeLink rawp
    , makeAuthor siteinfoAuthor
    , PubDate rawpostDate
    , Description
      (T.unpack . CM.commonmarkToHtml [] $ T.take 1000 rawpostContent)
    ]
  makeTitle = Title . T.unpack
  makeLink Rawpost {..} =
    Link
      .  fromJust
      .  parseURI
      $  T.unpack siteinfoUrl
      ++ "/"
      ++ htmlFilename (T.unpack rawpostTitle) rawpostDate
  makeAuthor = Author . T.unpack
