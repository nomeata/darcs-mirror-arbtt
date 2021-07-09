{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances, CPP #-}
module DumpFormat
    ( DumpFormat(..)
    , readDumpFormat
    , dumpActivity
    , dumpSample
    , dumpSamples
    ) where

import Data.MyText (unpack, null, pack, Text)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Time
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format(defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif
import Data.Char
import Data.Foldable (toList)
import Control.Applicative ((<$>), (<|>), (<*>), pure)

import Data
import Text.Printf
import Data.List hiding (null)
import Prelude hiding (null)

data DumpFormat
    = DFShow
    | DFHuman
    | DFJSON
    deriving (Show, Eq)

instance ToJSON Text where
    toJSON = toJSON . unpack
instance FromJSON Text where
    parseJSON x = pack <$> parseJSON x

instance ToJSON (TimeLogEntry CaptureData) where
    toJSON (TimeLogEntry {..}) = object [
        "date" .= tlTime,
        "rate" .= tlRate,
        "inactive" .= cLastActivity tlData,
        "windows" .= cWindows tlData,
        "desktop" .= cDesktop tlData,
        "screensaver" .= cScreenSaver tlData
        ]

instance FromJSON (TimeLogEntry CaptureData) where
    parseJSON = withObject "TimeLogEntry" $ \v -> do
        tlTime <- v .: "date"
        tlRate <- v .: "rate"
        cLastActivity <- v .: "inactive"
        cWindows  <- v .: "windows"
        cDesktop <- v .: "desktop"
        cScreenSaver <- v .: "screensaver" .!= False
        let tlData = CaptureData {..}
        let entry = TimeLogEntry {..}
        pure entry

instance ToJSON WindowData where
    toJSON WindowData{..} = object
        [ "active" .= wActive
        , "hidden" .= wHidden
        , "title" .= wTitle
        , "program" .= wProgram
        , "desktop" .= wDesktop
        ]

instance FromJSON WindowData where
    parseJSON = withObject "window" $ \v -> do
        wActive <- v .: "active"
        wHidden <- v .:! "hidden" .!= not wActive
        wTitle <- v .: "title"
        wProgram <- v .: "program"
        wDesktop <- v .:! "desktop" .!= ""
        pure WindowData{..}

readDumpFormat :: String -> Maybe DumpFormat
readDumpFormat arg =
    case map toLower arg of
        "human"      -> return DFHuman
        "show"       -> return DFShow
        "json"       -> return DFJSON
        _            -> Nothing

dumpActivity :: TimeLog (CaptureData, ActivityData) -> IO ()
dumpActivity = mapM_ go
 where
    go tle = do
        dumpHeader (tlTime tle) (cLastActivity cd) (cScreenSaver cd)
        dumpDesktop (cDesktop cd)
        mapM_ dumpWindow (cWindows cd)
        dumpTags ad
      where
        (cd, ad) = tlData tle

dumpTags :: ActivityData -> IO ()
dumpTags = mapM_ go
  where go act = printf "    %s\n" (show act)

dumpHeader :: UTCTime -> Integer -> Bool -> IO ()
dumpHeader time lastActivity screenSaver = do
    tz <- getCurrentTimeZone
    printf "%s (%dms inactive%s):\n"
        (formatTime defaultTimeLocale "%F %X" (utcToLocalTime tz time))
        lastActivity
        (if screenSaver then ", screen saver/locker active" else [])

dumpWindow :: WindowData -> IO ()
dumpWindow WindowData{..} = do
    printf "    (%c)%-*s %-15s %s\n" a (dw :: Int) d p t
  where a | wActive   = '*'
          | wHidden   = ' '
          | otherwise = '.'
        (dw, d) | wDesktop == "" = (0, "")
                | otherwise      = (15, " [" ++ unpack wDesktop  ++ "]")
        p = unpack wProgram ++ ":"
        t = unpack wTitle

dumpDesktop :: Text -> IO ()
dumpDesktop d
    | null d    = return ()
    | otherwise = printf "    Current Desktop: %s\n" (unpack d)

dumpSample :: TimeLogEntry CaptureData -> IO ()
dumpSample tle = do
    dumpHeader (tlTime tle) (cLastActivity (tlData tle)) (cScreenSaver (tlData tle))
    dumpDesktop (cDesktop (tlData tle))
    mapM_ dumpWindow (cWindows (tlData tle))

dumpSamples :: DumpFormat -> TimeLog CaptureData -> IO ()
dumpSamples DFShow = mapM_ print

dumpSamples DFHuman = mapM_ dumpSample

dumpSamples DFJSON = enclose . sequence_ . intersperse (putStrLn ",") . map (LBS.putStr . encode)
  where
    enclose m = putStrLn "[" >> m >> putStrLn "]"
