module BackupRotate.Errors (
  dateFileHandler
) where

import Data.Maybe
import System.IO.Error

import BackupRotate.Logger
import BackupRotate.Types

dateFileHandler :: IOError -> IO (Maybe Backup)
dateFileHandler ioe  = do
  let fname = fromMaybe "(unknown file)" $ ioeGetFileName ioe
  logE $ "Date file '" ++ fname ++ "' could not be read: " ++
         ioeGetErrorString ioe ++ ". The directory will not be processed" ++
         " and may get overwritten."
  return Nothing
