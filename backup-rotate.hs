--------------------------------------------------------------------------------
-- IMPORTS

-- Custom
import           Logger

-- Stdlib
import           Control.Monad                         (when)
import qualified Data.ByteString.Char8          as BS  (pack)
import           Data.List
import qualified Data.Map                       as Map
import           Data.Maybe
import           Data.Time
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Format
import           Text.Regex.PCRE.Light.Extra           ((=~))
import qualified System.Directory               as Dir
import           System.Environment
import           System.FilePath
import           System.IO
import           System.Locale

--------------------------------------------------------------------------------
-- CONSTANTS

backupRoot   = "/media/backup"
keepHourlies = 1 -- 12
keepDailies  = 1 -- 7
keepWeeklies = 1 -- 4
dateFormat   = "%Y-%m-%d %H:%M"

--------------------------------------------------------------------------------
-- MAIN AND I/O

main = do
  setupLog

  logI "Beginning rotation."
  logI $ "Processing directory '" ++ backupRoot ++ "'"

  bups' <- Dir.getDirectoryContents backupRoot
  bups  <- mapM dateForBackup $ backupDirs . sort $ bups'

  logI $ "Detected backups: " ++ (show bups)

  let dates    = map snd bups
      weeklies = filter (belongsTo $ weeklyDates dates) bups
      dailies  = filter (belongsTo $ dailyDates  dates) bups
      hourlies = filter (belongsTo $ hourlyDates dates) bups
      removals = filter (belongsTo $ toRemove    dates) bups

  logI $ "Detected weeklies: "          ++ (show weeklies)
  logI $ "Detected dailies: "           ++ (show dailies)
  logI $ "Detected hourlies: "          ++ (show hourlies)
  logI $ "Detected backups to remove: " ++ (show removals)

  logI "Moving backups to temporary folders to prevent overwriting..."
  mvtemp bups

  logI "Enumerating folders..."

  enumerate "weekly" $ weeklies
  enumerate "daily"  $ dailies
  enumerate "hourly" $ hourlies

  logI "Removing obsolete folders..."

  remove removals

  logI "Rotation done."
-- end main

mvtemp :: [(FilePath, LocalTime)] -> IO ()
mvtemp []     = return ()
mvtemp (x:xs) = do mv path (temp path)
                   mvtemp xs
                where path = expand . fst $ x

enumerate :: String -> [(FilePath, LocalTime)] -> IO ()
enumerate prefix [] = return ()
enumerate prefix xs = do mv src dest
                         enumerate prefix $ tail xs
                      where src  = temp . expand . fst . last $ xs
                            dest = expand (prefix ++ '.':no)
                            no   = show . length . init $ xs

remove :: [(FilePath, LocalTime)] -> IO ()
remove []     = return ()
remove (x:xs) = do rm_r path
                   remove xs
                where path = temp . expand . fst $ x

dateForBackup :: FilePath -> IO (FilePath, LocalTime)
dateForBackup dir = do
  date <- readFile $ (expand dir) </> "date"
  return (dir, strToDate date)

--------------------------------------------------------------------------------
-- FUNCTIONS

-- I/O-related

backupDirs :: [FilePath] -> [FilePath]
backupDirs [] = []
backupDirs xs = filter (regexp "^(hourly|daily|weekly)\\.[0-9]$") xs

strToDate :: String -> LocalTime
strToDate s =
  fromJust (parseTime defaultTimeLocale dateFormat s :: Maybe LocalTime)


-- Backups to be retained for each interval
--
-- For each interval (weekly, daily, hourly), the corresponding function filters
-- a list of timestamps and selects those timestamps that shall be retained for
-- the interval. These functions already take other intervals into account,
-- whereby long-term intervals take precedence over short-term intervals. For
-- instance:
--
-- 1. If one backup has been made in one week, there will be one weekly.
-- 2. If two backups have been made in one week, there will be one weekly and
--    one daily.
-- 3. If four backups have been made in one week, there will be one weekly,
--    one daily and two hourlies.
--
-- Note that at the hourly level, we don't check whether or not two backups
-- have been created at different hours of the day. Hence, two backups made at
-- 12:00 and 12:01 respectively will be kept as two 'hourlies'.
weeklyDates, dailyDates, hourlyDates :: [LocalTime] -> [LocalTime]

weeklyDates []    = []
weeklyDates dates = take keepWeeklies uniqueWDays
                 where uniqueWDays = map head $ groupBy cWeek $ reverse . sort $  dates
                       cWeek x y   = ((localWeek x) == (localWeek y)) &&
                                     ((localYear x) == (localYear y))

dailyDates  []    = []
dailyDates  dates = take keepDailies uniqueDays
                 where uniqueDays  = map head $ groupBy cDay  $ reverse . sort $ (dates \\ (weeklyDates dates))
                       cDay x y    = (diffDays (localDay x) (localDay y)) == 0

hourlyDates []    = []
hourlyDates dates = take keepHourlies $ reverse . sort $ (dates \\ (weeklyDates dates ++ dailyDates dates))

-- Predicate for the intervals
--
-- Tests if a backup (which is a tuple containing a path and a timestamp)
-- belongs to the specified interval. The interval is given as a list of
-- timestamps that belong to it.
belongsTo :: [LocalTime] -> (FilePath, LocalTime) -> Bool
belongsTo int bup = (snd bup) `elem` int

-- Backups to be removed
--
-- This is equivalent to 'all backups that do not belong to one of the
-- intervals'.
toRemove :: [LocalTime] -> [LocalTime]
toRemove dates =
  dates \\ ((weeklyDates dates) ++ (dailyDates dates) ++ (hourlyDates dates))

-- HELPERS

localWeek :: LocalTime -> Int
localWeek = fst . mondayStartWeek . localDay

localYear :: LocalTime -> Integer
localYear = year . toGregorian . localDay
            where year (y,m,d) = y

regexp :: String -> String -> Bool
regexp pat str = (BS.pack str) =~ (BS.pack pat) :: Bool

mv :: FilePath -> FilePath -> IO ()
mv src dest = do logI $ "mv '" ++ src ++ "' '" ++ dest ++ "'"
                 -- renameDirectory src dest
                 return ()

rm_r :: FilePath -> IO ()
rm_r dir = do logI $ "rm -r '" ++ dir ++ "'"
              -- removeDirectory dir
              return ()

temp :: FilePath -> FilePath
temp path = path ++ ".tmp"

expand :: FilePath -> FilePath
expand src = backupRoot </> src
