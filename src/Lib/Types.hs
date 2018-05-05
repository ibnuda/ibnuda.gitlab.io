module Lib.Types where

import           Data.Time
import           Data.Time.Clock.POSIX
import           Lib.Prelude

data SiteInfo = SiteInfo
  { siteinfoUrl    :: Text
  , siteinfoName   :: Text
  , siteinfoAuthor :: Text
  , siteinfoFiles  :: Filename
  , siteinfoPublic :: Filename
  , siteinfoRSS    :: Filename
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

