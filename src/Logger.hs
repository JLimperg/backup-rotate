module Logger
( setupLog
, logD
, logI
, logW
, logE
) where

import           Data.Time
import           Data.Time.Format

import           System.Locale
import           System.Log.Logger
import qualified System.Log.Handler.Simple      as LogHandler (streamHandler)

setupLog :: IO ()
setupLog = do
  updateGlobalLogger "log"
                     (setLevel INFO)
  return ()

logC :: Priority -> String -> IO ()
logC prio msg = do
  date <- getCurrentTime
  let pre     = dateStr ++ " backup_rotate: " ++ '[':prioStr:']':" "
      dateStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" date
      prioStr = head $ show prio
  logM "log" prio $ pre ++ msg
  return ()

logD, logI, logW, logE :: String -> IO ()
logD = logC DEBUG
logI = logC INFO
logW = logC WARNING
logE = logC ERROR
