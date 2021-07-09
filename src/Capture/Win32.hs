module Capture.Win32 where

import Data
import qualified Data.MyText as T
import Control.Monad
import Control.Exception (bracket)
import Control.Applicative
import Data.Maybe
import Data.Time.Clock
import System.IO

import Graphics.Win32.Window.Extra

setupCapture :: IO ()
setupCapture = do
        return ()

captureData :: IO CaptureData
captureData = do
        titles <- fetchWindowTitles
        foreground <- getForegroundWindow

        let winData = [ fromWDv0 (h == foreground, T.pack t, T.pack p)
                      | (h, t, p) <- titles]

        it <- fromIntegral `fmap` getIdleTime
        -- TODO: screen saver/locker

        return $ CaptureData winData it (T.pack "") False
