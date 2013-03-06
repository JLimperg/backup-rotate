-- Copyright (c) 2013 Jannis Limperg
-- License: MIT. See the LICENSE file.

module BackupRotate.Main where

--------------------------------------------------------------------------------
-- IMPORTS

import           Control.Monad                         (liftM)
import qualified Data.ByteString.Char8          as BS  (pack)
import           Data.Function                         (on)
import           Data.List
import           Data.Maybe
import           Data.Time
import           Data.Time.Calendar.OrdinalDate        (mondayStartWeek)
import           Text.Regex.PCRE.Light.Extra           ((=~))
import qualified System.Directory               as Dir
import           System.FilePath                       ((</>), splitPath)
import           System.IO.Error                       (catchIOError)
import           System.Locale                         (defaultTimeLocale)

import           BackupRotate.Errors            as E
import           BackupRotate.Logger
import           BackupRotate.Types

--------------------------------------------------------------------------------
-- CONSTANTS

backupRoot :: FilePath
backupRoot = "/media/backup"

dateFormat :: String
dateFormat = "%Y-%m-%d %H:%M"

intervals :: [Interval]
intervals = [ Interval {iName = "hourly", iPredicate = hourlyP, iKeep = 12}
            , Interval {iName = "daily" , iPredicate = dailyP , iKeep = 7 }
            , Interval {iName = "weekly", iPredicate = weeklyP, iKeep = 4 }
            ]

hourlyP, dailyP, weeklyP :: Backup -> Backup -> Bool
hourlyP = timeP [ (==) `on` localHour
                , (==) `on` localDay
                , (==) `on` localYear
                ]
dailyP  = timeP [ (==) `on` localDay
                , (==) `on` localYear
                ]
weeklyP = timeP [ (==) `on` localWeek
                , (==) `on` localYear
                ]

--------------------------------------------------------------------------------
-- MAIN AND I/O

main :: IO ()
main = do
    setupLog

    logI "Beginning rotation."
    logI $ "Processing directory '" ++ backupRoot ++ "'"

    logI $ "Detecting backups according to basename pattern " ++ backupDirPat
    bups' <- Dir.getDirectoryContents backupRoot
    logI $ "Possible backup directories: " ++ show bups'
    bups  <- liftM catMaybes $ mapM dateForBackup $ backupDirs . sort $ bups'
    logI $ "Detected backups: " ++ show bups

    let msg i = "Detected " ++ iName i ++ " backups: " ++
                show (keepBups bups i)
    mapM_ (logI . msg) intervals

    logI "Moving backups to temporary folders to prevent overwriting..."
    mvtemp bups

    logI "Enumerating folders..."
    mvall $ enumerate bups

    logI "Removing obsolete folders..."
    remove $ toRemove bups

    logI "Rotation done."
-- end main

mvtemp :: [Backup] -> IO ()
mvtemp =
    mapM_ $ \p -> mv (path p) (temp . path $ p)
  where
    path = expand . bupPath

mvall :: [(FilePath, FilePath)] -> IO ()
mvall = mapM_ (uncurry mv)

remove :: [Backup] -> IO ()
remove = mapM_ $ rmR . temp . expand . bupPath

dateForBackup :: FilePath -> IO (Maybe Backup)
dateForBackup dir =
    readDate `catchIOError` E.dateFileHandler
  where
    readDate =
      readFile (expand dir </> "date") >>= \date ->
      return $ Just (dir, strToDate date)

--------------------------------------------------------------------------------
-- FUNCTIONS

-- I/O-related

backupDirPat :: String
backupDirPat =
    "^(" ++ iNames ++ ")\\.[0-9]+$"
  where
    iNames = intercalate "|" $ map iName intervals

backupDirs :: [FilePath] -> [FilePath]
backupDirs xs = filter (regexp backupDirPat) (map (last . splitPath) xs)

strToDate :: String -> LocalTime
strToDate s =
    fromJust (parseTime defaultTimeLocale dateFormat s :: Maybe LocalTime)

enumerate :: [Backup] -> [(FilePath, FilePath)]
enumerate []   = []
enumerate bups =
    concatMap (\i -> enumerateInterval (iName i) (keepBups bups i)) intervals

-- requires a sorted list of backups!
enumerateInterval :: String -> [Backup] -> [(FilePath, FilePath)]
enumerateInterval _      []   = []
enumerateInterval prefix bups =
    (src, newpath prefix no) : enumerateInterval prefix (init bups)
  where
    newpath p no' = expand $ p ++ '.':no'
    src           = temp . expand . bupPath . last $ bups
    no            = show . length . init $ bups

-- Backups to be retained for each interval
--
-- For an explanation of the algorithm, see the README.
keepBups :: [Backup] -> Interval -> [Backup]
keepBups []   _ = []
keepBups bups i =
    take (iKeep i) $ reverse intervalBups
  where
    intervalBups    = map head $ groupBy (iPredicate i) freeBups
    freeBups        = sortBups $ bups \\ unfreeBups
    unfreeBups      = concatMap (keepBups bups) higherIntervals
    higherIntervals = drop (fromJust (elemIndex i intervals) + 1) intervals

-- Backups to be removed
--
-- This is equivalent to 'all backups that do not belong to one of the
-- intervals'.
toRemove :: [Backup] -> [Backup]
toRemove bups = bups \\ concatMap (keepBups bups) intervals

-- HELPERS

sortBups :: [Backup] -> [Backup]
sortBups = sortBy (compare `on` snd)

localHour, localWeek :: LocalTime -> Int
localWeek = fst . mondayStartWeek . localDay
localHour = todHour . localTimeOfDay

localYear :: LocalTime -> Integer
localYear = (\(y,_,_) -> y) . toGregorian . localDay

regexp :: String -> String -> Bool
regexp pat str = BS.pack str =~ BS.pack pat :: Bool

mv :: FilePath -> FilePath -> IO ()
mv src dest = do
    logI $ "mv '" ++ src ++ "' '" ++ dest ++ "'"
    Dir.renameDirectory src dest

rmR :: FilePath -> IO ()
rmR dir = do
    logI $ "rm -r '" ++ dir ++ "'"
    Dir.removeDirectoryRecursive dir

temp :: FilePath -> FilePath
temp path = path ++ ".tmp"

expand :: FilePath -> FilePath
expand src = backupRoot </> src

timeP :: [LocalTime -> LocalTime -> Bool] -> Backup -> Backup -> Bool
timeP fs a b = all (\f -> f (bupTime a) (bupTime b)) fs
