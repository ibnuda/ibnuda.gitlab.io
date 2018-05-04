module Lib.Types where

import           Data.Time
import           Data.Time.Clock.POSIX
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

data SiteInfo = SiteInfo
  { siteinfoUrl    :: Text
  , siteinfoName   :: Text
  , siteinfoAuthor :: Text
  , siteinfoFiles  :: FilePath
  , siteinfoPublic :: FilePath
  } deriving (Show, Eq)

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

