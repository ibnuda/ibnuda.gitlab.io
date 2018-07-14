{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Lib.Types where

import           Protolude

import           Data.Text
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Yaml

data SiteInfo = SiteInfo
  { siteinfoUrl          :: Text
  , siteinfoName         :: Text
  , siteinfoAuthor       :: Text
  , siteinfoFiles        :: Filename
  , siteinfoPublic       :: Filename
  , siteinfoRSS          :: Filename
  , siteinfoCSS          :: Filename
  , siteinfoTemplatesDir :: Filename
  } deriving (Show, Eq)

instance FromJSON SiteInfo where
  parseJSON = withObject "SiteInfo" $ \o -> do
    siteinfoUrl <- o .: "url"
    siteinfoName <- o .: "name"
    siteinfoAuthor <- o .: "author"
    siteinfoFiles <- o .: "files"
    siteinfoPublic <- o .: "public"
    siteinfoRSS <- o .: "rss"
    siteinfoCSS <- o .: "css"
    siteinfoTemplatesDir <- o .: "templates"
    return SiteInfo {..}

readSiteinfo :: IO SiteInfo
readSiteinfo = do
  eith <- decodeFileEither "res/config.yaml"
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

data BlogLink = BlogLink
  { menulinkurl   :: Text,
    menulinktitle :: Text
  } deriving (Generic)
instance ToJSON BlogLink

data BlogPage = BlogPage
  { pagetitle   :: Text
  , pagecss     :: Text
  , pagedesc    :: Text
  , pagecontent :: Text
  , pagelinks   :: [BlogLink]
  } deriving (Generic)
instance ToJSON BlogPage

data Index = Index
  { indextitle   :: Text
  , indexcss     :: Text
  , indexdesc    :: Text
  , indexlinks   :: [BlogLink]
  , indexindice  :: [BlogLink]
  } deriving (Generic)
instance ToJSON Index
