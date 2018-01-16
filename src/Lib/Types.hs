module Lib.Types where

import           Control.Applicative
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Yaml
import           Lib.Prelude

data Configuration = Configuration
  { siteURL       :: Text
  , siteName      :: Text
  , author        :: Text
  , pathPages     :: Text
  , pathPosts     :: Text
  , pathGenerated :: Text
  } deriving (Show, Eq)

data Content = Content
  { filename    :: Text
  , mdTitle     :: Text
  , mdDate      :: UTCTime
  , contentType :: Text
  , contentText :: Text
  } deriving (Show, Eq)

instance Ord Content where
  (Content _ _ d1 _ _) `compare` (Content _ _ d2 _ _) =
    d1 `compare` d2

emptyContent :: Content
emptyContent = Content "" "" (posixSecondsToUTCTime 0) "" ""

instance FromJSON Configuration where
  parseJSON (Object o) =
    Configuration <$>
    o .: "url" <*>
    o .: "name" <*>
    o .: "author" <*>
    o .: "pages" <*>
    o .: "posts" <*>
    o .: "generated"
  parseJSON _ = undefined
