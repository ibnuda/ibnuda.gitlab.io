{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.RSS
-- Copyright   :  Copyright 2004, Jeremy Shaw, http://www.n-heptane.com/
--                Copyright 2004-2006, Bjorn Bringert (bjorn@bringert.net)
-- License     :  This code is released to the public domain and comes
--                with no warranty.
--
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- A libary for generating RSS 2.0 feeds.
--
--
-- Original module by Jeremy Shaw.
--
-- Changes by Bjorn Bringert:
--
-- * showXml just converts the RSS to a [Char], does not print it.
--
-- * Added XML escaping.
--
-- * Use RFC 2822 format for dates.
--
-- * Added all elements from RSS 2.0.1-rv-6,
--   <http://www.rssboard.org/rss-2-0-1-rv-6>
--
-- * Use HaXml.Verbatim instead of HaXml.Pretty, since
--   HaXml.Pretty seems to introduce spaces around entities.
--
-- * Removed the use of content:encoded, since the description
--   tag is the recommented way to include HTML content in RSS 2.0.
--
-- Changes by Bas van Dijk:
--
-- * Use @UTCTime@ from @time@ instead of @CalendarTime@ from @old-time@.
--
-- * Add our own @Weekday@ type instead of using the @Day@ type from @old-time@.
--
-- Changes by Ibnu D. Aji:
--
-- * Delete everything.
-----------------------------------------------------------------------------
module Lib.RSS where

import           Lib.Prelude

import           Data.Ix                    (Ix)
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Format           (defaultTimeLocale,
                                             rfc822DateFormat)
import           Data.Time.Format           (formatTime)
import           Network.URI                (URI)
import           Text.XML.HaXml.Combinators (CFilter, cdata, literal, mkElem,
                                             mkElemAttr)
import           Text.XML.HaXml.Escape      (stdXmlEscaper, xmlEscape)
import           Text.XML.HaXml.Types       (Content (..), Element)
import           Text.XML.HaXml.Verbatim    (verbatim)

data RSS = RSS
  { rssTitle :: RTitle
  , rssLink  :: Link
  , rssDesc  :: Description
  , rssElem  :: [ChannelElem]
  , rssItem  :: [Item]
  } deriving (Show)

type Item = [ItemElem]

type RTitle = [Char]
type Link = URI
type Description = [Char]
type Email = [Char]
type Domain = [Char]
type Hour = Int
type Minutes = Int

data ChannelElem
  = Language [Char]
  | Copyright [Char]
  | ManagingEditor Email
  | WebMaster Email
  | ChannelPubDate UTCTime
  | LastBuildDate UTCTime
  | Generator [Char]
  | TTL Minutes
  | SkipHours [Hour]
  | SkipDays [Weekday]
  deriving (Show)

data ItemElem
  = Title RTitle
  | Link Link
  | Description Description
  | Author Email
  | Comments URI
  | PubDate UTCTime
  | Source URI
           RTitle
  | Guid Bool [Char]
  deriving (Show)

-- | A day of the week.
data Weekday
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

-- | Converts RSS to XML.
rssToXML :: RSS -> CFilter ()
rssToXML (RSS title l description celems items) =
  mkElemAttr
    "rss"
    [("version", literal "2.0")]
    [ mkElem
        "channel"
        ([mkTitle title, mkLink l, mkDescription description, mkDocs] ++
         map mkChannelElem celems ++ map mkItem items)
    ]

-- | Render XML as a string.
showXML :: CFilter () -> [Char]
showXML = verbatim . cfilterToElem

cfilterToElem :: CFilter () -> Element ()
cfilterToElem f =
  case f (CString False "" ()) of
    [CElem e _] -> xmlEscape stdXmlEscaper e
    []          -> panic "RSS produced no output"
    _           -> panic "RSS produced more than one output"

mkSimple :: [Char] -> [Char] -> CFilter ()
mkSimple t str = mkElem t [literal str]

mkTitle :: RTitle -> CFilter ()
mkTitle = mkSimple "title"

mkLink :: Link -> CFilter ()
mkLink = mkSimple "link" . show

mkDescription :: Description -> CFilter ()
mkDescription str = mkElem "description" [cdata str]

mkDocs :: CFilter ()
mkDocs = mkSimple "docs" "http://www.rssboard.org/rss-specification"

mkPubDate :: UTCTime -> CFilter ()
mkPubDate = mkSimple "pubDate" . formatDate

formatDate :: UTCTime -> [Char]
formatDate = formatTime defaultTimeLocale rfc822DateFormat

maybeElem :: (a -> CFilter ()) -> Maybe a -> [CFilter ()]
maybeElem = maybe [] . ((:[]) .)

mkChannelElem :: ChannelElem -> CFilter ()
mkChannelElem (Language str) = mkSimple "language" str
mkChannelElem (Copyright str) = mkSimple "copyright" str
mkChannelElem (ManagingEditor str) = mkSimple "managingEditor" str
mkChannelElem (WebMaster str) = mkSimple "webMaster" str
mkChannelElem (ChannelPubDate date) = mkPubDate date
mkChannelElem (LastBuildDate date) = mkSimple "lastBuildDate" $ formatDate date
mkChannelElem (Generator str) = mkSimple "generator" str
mkChannelElem (TTL minutes) = mkSimple "ttl" $ show minutes
mkChannelElem (SkipHours hs) = mkElem "skipHours" (map (mkSimple "hour" . show) hs)
mkChannelElem (SkipDays ds) = mkElem "skipDays" (map (mkSimple "day" . show) ds)

mkItem :: Item -> CFilter ()
mkItem itemElems = mkElem "item" (map mkItemElem itemElems)

mkItemElem :: ItemElem -> CFilter ()
mkItemElem (Title t) = mkTitle t
mkItemElem (Link l) = mkLink l
mkItemElem (Description d) = mkDescription d
mkItemElem (Author e) = mkElem "author" [literal e]
mkItemElem (Comments uri) = mkSimple "comments" $ show uri
mkItemElem (Guid perm s) = mkElemAttr "guid" attrs [literal s]
  where
    attrs =
      if perm
        then [("isPermalik", literal "true")]
        else []
mkItemElem (PubDate ct) = mkElem "pubDate" [ literal (formatDate ct) ]
mkItemElem (Source uri t) =
  mkElemAttr "source" [("url", literal (show uri))] [literal t]
