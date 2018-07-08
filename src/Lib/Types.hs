{-# LANGUAGE RecordWildCards #-}
module Lib.Types where

import           Lib.Prelude

import           Data.Text
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Yaml

data SiteInfo = SiteInfo
  { siteinfoUrl    :: Text
  , siteinfoName   :: Text
  , siteinfoAuthor :: Text
  , siteinfoFiles  :: Filename
  , siteinfoPublic :: Filename
  , siteinfoRSS    :: Filename
  } deriving (Show, Eq)

instance FromJSON SiteInfo where
  parseJSON = withObject "SiteInfo" $ \o -> do
    siteinfoUrl <- o .: "url"
    siteinfoName <- o .: "name"
    siteinfoAuthor <- o .: "author"
    siteinfoFiles <- o .: "files"
    siteinfoPublic <- o .: "public"
    siteinfoRSS <- o .: "rss"
    return SiteInfo {..}


readSiteinfo :: IO SiteInfo
readSiteinfo = do
  eith <- decodeFileEither "config.yaml"
  case eith of
    Left  e -> panic . pack . prettyPrintParseException $ e
    Right s -> pure s

type Title = Text
type Filename = FilePath
type MdContent = Text
data PostType = Post | Page deriving (Eq, Read, Show)

data Rawpost = Rawpost
  { rawpostFilename :: Filename
  , rawpostTitle    :: Title
  , rawpostDate     :: UTCTime
  , rawpostType     :: PostType
  , rawpostContent  :: MdContent
  } deriving (Eq, Show)

defaultRawpost :: Rawpost
defaultRawpost = Rawpost "" "" (posixSecondsToUTCTime 0) Post ""

instance Ord Rawpost where
  compare a b = compare (rawpostDate a) (rawpostDate b)

