module UpgradeLog1 (upgradeLogFile1) where

import qualified Data.ByteString.Char8 as BS
import System.IO
import Data.Time
import Control.Applicative
import Control.Monad
import System.Directory
import qualified Data.MyText as T

import TimeLog (writeTimeLog)
import qualified Data as D

-- | A copy of the data definitions as they were on 2009-10-03, to be able to
-- change them in the main code later, and still be able to read old log files.

type TimeLog a = [TimeLogEntry a]

data TimeLogEntry a = TimeLogEntry
        { tlTime :: UTCTime
        , tlRate :: Integer -- ^ in milli-seconds
        , tlData :: a }
  deriving (Show, Read)

instance Functor TimeLogEntry where
        fmap f tl = tl { tlData = f (tlData tl) }

data CaptureData = CaptureData
        { cWindows :: [ (Bool, String, String) ]
                -- ^ Active window, window title, programm name
        , cLastActivity :: Integer -- ^ in milli-seconds
        }
  deriving (Show, Read)

readTimeLog :: Read a => FilePath -> IO (TimeLog a)
readTimeLog filename = (map (read.read) . lines) <$> (openFile filename ReadMode >>= hGetContents)

magicStart = BS.pack "\"TimeLogEntry"

upgradeLogFile1 captureFile = do
        ex <- doesFileExist captureFile
        when ex $ do
                h <- openFile captureFile ReadMode      
                start <- BS.hGet h (BS.length magicStart)
                hClose h
                when (start == magicStart) $ do
                        putStrLn $ "Detected old text file format. Creating backup at " ++
                                    oldFile ++ " and converting to new format..."
                        renameFile captureFile oldFile
                        captures <- readTimeLog oldFile
                        writeTimeLog captureFile (upgrade captures)
                        putStrLn   "done."

 where oldFile = captureFile ++ ".old"

upgrade :: TimeLog CaptureData -> D.TimeLog D.CaptureData
upgrade = map $ \(TimeLogEntry a b c) -> D.TimeLogEntry a b (upgradeCD c)

upgradeCD :: CaptureData -> D.CaptureData
upgradeCD (CaptureData a b) = D.CaptureData (map upgrageWD a) b (T.pack "") False
 where upgrageWD (b, s1, s2) = D.fromWDv0 (b, T.pack s1, T.pack s1)


