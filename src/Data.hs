{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data where

import GHC.Generics (Generic)
import Data.Time
import Text.ParserCombinators.ReadPrec (readP_to_Prec)
import Text.ParserCombinators.ReadP hiding (get)
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read (readPrec)
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.StringRef
import Data.Bits
import qualified Data.MyText as T
import Data.MyText (Text)
import Control.Applicative
import Control.Monad
import Control.DeepSeq

type TimeLog a = [TimeLogEntry a]

data TimeLogEntry a = TimeLogEntry
        { tlTime :: UTCTime
        , tlRate :: Integer -- ^ in milli-seconds
        , tlData :: a }
  deriving (Show, Read, Functor, Generic, NFData)

data CaptureData = CaptureData
        { cWindows :: [WindowData]
        , cLastActivity :: Integer -- ^ in milli-seconds
        , cDesktop :: Text
                -- ^ Current desktop name
        , cScreenSaver :: Bool -- ^ Screen saver or locker active?
        }
  deriving (Show, Read, Generic, NFData)

data WindowData = WindowData
        { wActive :: Bool
        , wHidden :: Bool
        , wTitle :: Text
        , wProgram :: Text
        , wDesktop :: Text
        }
  deriving (Show, Read, Generic, NFData)

type ActivityData = [Activity]

data Activity = Activity 
        { activityCategory :: Maybe Category
        , activityName :: Text
        }
  deriving (Ord, Eq, Generic, NFData)

-- | An activity with special meaning: ignored by default (i.e. for idle times)
inactiveActivity = Activity Nothing "inactive"

instance Show Activity where
 show (Activity mbC t) = maybe "" ((++":").T.unpack) mbC ++ (T.unpack t)

instance Read Activity where
 readPrec = readP_to_Prec $ \_ ->
                   (do cat <- munch1 (/= ':')
                       char ':'
                       tag <- many1 ReadP.get
                       return $ Activity (Just (T.pack cat)) (T.pack tag))
                   <++ (Activity Nothing . T.pack <$> many1 ReadP.get)

type Category = Text

isCategory :: Category -> Activity -> Bool
isCategory cat (Activity (Just cat') _) = cat == cat'
isCategory _   _                        = False


-- Data.Binary instances

validTimeLogEntryTag :: Word8 -> Bool
validTimeLogEntryTag 1 = True
validTimeLogEntryTag _ = False

instance StringReferencingBinary a => StringReferencingBinary (TimeLogEntry a) where
 ls_put strs tle = do
        -- A version tag
        putWord8 1
        put (tlTime tle)
        put (tlRate tle)
        ls_put strs (tlData tle)
 ls_get strs = do
        v <- getWord8
        case v of
         1 -> TimeLogEntry <$> get <*> get <*> ls_get strs
         _ -> error $ "Unsupported TimeLogEntry version tag " ++ show v ++ "\n" ++
                      "You can try to recover your data using arbtt-recover."

instance Binary UTCTime where
 put (UTCTime (ModifiedJulianDay d) t) = do
        put d
        put (toRational t)
 get = do
        d <- get
        t <- get
        return $ UTCTime (ModifiedJulianDay d) ({-# SCC diffTimeFromRational #-} fromRational t)

instance ListOfStringable CaptureData where
  -- backward compat hack: skip empty wDesktop to keep original order,
  -- but add an empty string at the end to compact empty strings as well
  listOfStrings cd = concatMap listW (cWindows cd) ++ [""]
    where listW wd = [wTitle wd, wProgram wd]
                  ++ [wDesktop wd | wDesktop wd /= ""]

instance StringReferencingBinary CaptureData where
-- Versions:
-- 1 First version
-- 2 Using ListOfStringable
-- 3 Add cDesktop
-- 4 WindowData instead of 3-tuple; CompactNum
-- 5 Add cScreenSaver
 ls_put strs cd = do
        -- A version tag
        putWord8 5
        ls_put strs (cWindows cd)
        ls_put strs (cLastActivity cd)
        ls_put strs (cDesktop cd)
        ls_put strs (cScreenSaver cd)
 ls_get strs = do
        v <- getWord8
        case v of
         1 -> CaptureData <$> (map fromWDv0 . fromIntLenW <$> get) <*> get <*> pure "" <*> pure False
         2 -> CaptureData <$> (map fromWDv0 . fromIntLenW <$> ls_get strs) <*> ls_get strs <*> pure "" <*> pure False
         3 -> CaptureData <$> (map fromWDv0 . fromIntLenW <$> ls_get strs) <*> ls_get strs <*> (fromIntLen <$> ls_get strs) <*> pure False
         4 -> CaptureData <$> ls_get strs <*> ls_get strs <*> ls_get strs <*> pure False
         5 -> CaptureData <$> ls_get strs <*> ls_get strs <*> ls_get strs <*> ls_get strs
         _ -> error $ "Unsupported CaptureData version tag " ++ show v ++ "\n" ++
                      "You can try to recover your data using arbtt-recover."

fromIntLenW :: IntLen [(Bool, IntLen Text, IntLen Text)] -> [(Bool, Text, Text)]
fromIntLenW ws = [(a, t, p) | (a, IntLen t, IntLen p) <- fromIntLen ws]

fromWDv0 :: (Bool, Text, Text) -> WindowData
fromWDv0 (a, t, p) = WindowData{
  -- wHidden = not wActive for old data, so that rules that look at visible
  -- windows don't misfire; uncategorized is better than categorized wrong
  wActive = a, wHidden = not a, wTitle = t, wProgram = p, wDesktop = "" }

instance StringReferencingBinary WindowData where
-- Versions:
-- 0 3-tuple without version tag, handled in `instance StringReferencingBinary CaptureData`
-- 1 WindowData record; Added wHidden, wDesktop; CompactNum; bitfield
  ls_put strs WindowData{..} = do
        putWord8 1
        putWord8 ((if wActive then bit 0 else 0) .|. (if wHidden then bit 1 else 0))
        ls_put strs wTitle
        ls_put strs wProgram
        ls_put strs wDesktop
  ls_get strs = do
        v <- getWord8
        case v of
         1 -> do
             bits <- getWord8
             let wActive = testBit bits 0
             let wHidden = testBit bits 1
             wTitle <- ls_get strs
             wProgram <- ls_get strs
             wDesktop <- ls_get strs
             return WindowData{..}
         _ -> error $ "Unsupported WindowData version tag " ++ show v ++ "\n" ++
                      "You can try to recover your data using arbtt-recover."
