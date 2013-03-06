-- Copyright (c) 2013 Jannis Limperg
-- License: MIT. See the LICENSE file.

module BackupRotate.Logger (
  setupLog
, logD
, logI
, logW
, logE
) where

import Data.Time
import System.Locale     (defaultTimeLocale)
import System.Log.Logger

setupLog :: IO ()
setupLog = updateGlobalLogger "log" (setLevel INFO)

logC :: Priority -> String -> IO ()
logC prio msg = do
    date <- getCurrentTime
    let pre     = dateStr ++ " backup_rotate: " ++ '[':prioStr:']':" "
        dateStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" date
        prioStr = head $ show prio
    logM "log" prio $ pre ++ msg

logD, logI, logW, logE :: String -> IO ()
logD = logC DEBUG
logI = logC INFO
logW = logC WARNING
logE = logC ERROR
