--------------------------------------------------------------------------------
-- IMPORTS

-- Custom
import           Logger

-- Stdlib
import           Control.Monad                         (when)
import qualified Data.ByteString.Char8          as BS  (pack)
import           Data.Function
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
import           System.Locale                         (defaultTimeLocale)

--------------------------------------------------------------------------------
-- CONSTANTS

backupRoot   = "/media/backup"
dateFormat   = "%Y-%m-%d %H:%M"
keepHourlies = 12
keepDailies  = 7
keepWeeklies = 4

intervals = [ Interval {iName = "hourly", iPredicate = hourlyP, keepFromInterval = 12}
            , Interval {iName = "daily" , iPredicate = dailyP , keepFromInterval = 7 }
            , Interval {iName = "weekly", iPredicate = weeklyP, keepFromInterval = 4 }
            ]

hourlyP = timeP [ \a b -> (localHour a) == (localHour b)
                , \a b -> (localDay  a) == (localDay  b)
                , \a b -> (localYear a) == (localYear b)
                ]
dailyP  = timeP [ \a b -> (localDay  a) == (localDay  b)
                , \a b -> (localYear a) == (localYear b)
                ]
weeklyP = timeP [ \a b -> (localWeek a) == (localWeek b)
                , \a b -> (localYear a) == (localYear b)
                ]

--------------------------------------------------------------------------------
-- TYPES

-- Backup
type Backup = (FilePath, LocalTime)

bupPath :: Backup -> FilePath
bupPath = fst

bupTime :: Backup -> LocalTime
bupTime = snd

-- Interval
data Interval = Interval { iName      :: String
                         , iPredicate :: (Backup -> Backup -> Bool)
                         , keepFromInterval  :: Int
                         }

instance Eq Interval where
  a == b = (iName a) == (iName b)

instance Show Interval where
  show i = '(':(iName i) ++ ' ':(show $ keepFromInterval i) ++ ")"


--------------------------------------------------------------------------------
-- MAIN AND I/O

main = do
  setupLog

  logI "Beginning rotation."
  logI $ "Processing directory '" ++ backupRoot ++ "'"

  bups' <- Dir.getDirectoryContents backupRoot
  bups  <- mapM dateForBackup $ backupDirs . sort $ bups'

  logI $ "Detected backups: " ++ (show bups)
  let msg i = "Detected " ++ iName i ++ " backups: " ++ (show paths)
              where paths = keepBups bups i
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
mvtemp []     = return ()
mvtemp (x:xs) = do mv path (temp path)
                   mvtemp xs
                where path = expand . bupPath $ x

mvall :: [(FilePath, FilePath)] -> IO ()
mvall paths = do mapM_ (\(src, dest) -> mv src dest) paths
                 return ()


remove :: [Backup] -> IO ()
remove []     = return ()
remove (x:xs) = do rm_r path
                   remove xs
                where path = temp . expand . bupPath $ x

dateForBackup :: FilePath -> IO (Backup)
dateForBackup dir = do
  date <- readFile $ (expand dir) </> "date"
  return (dir, strToDate date)

--------------------------------------------------------------------------------
-- FUNCTIONS

-- I/O-related

backupDirs :: [FilePath] -> [FilePath]
backupDirs xs = filter (regexp "^(hourly|daily|weekly)\\.[0-9]$") xs

strToDate :: String -> LocalTime
strToDate s =
  fromJust (parseTime defaultTimeLocale dateFormat s :: Maybe LocalTime)

enumerate :: [Backup] -> [(FilePath, FilePath)]
enumerate []    = []
enumerate bups  = concat $
                  map (\i -> enumerateInterval (prefix i) (iBups i)) intervals
                  where iBups  = sortBups . keepBups bups
                        prefix = iName

-- requires a sorted list of backups!
enumerateInterval :: String -> [Backup] -> [(FilePath, FilePath)]
enumerateInterval prefix []     = []
enumerateInterval prefix bups =
  (src, newpath prefix no):(enumerateInterval prefix (init bups))
  where newpath p no = expand $ p ++ '.':no
        src  = temp . expand . bupPath . last $ bups
        no   = show . length . init $ bups

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
keepBups :: [Backup] -> Interval -> [Backup]
keepBups []   _ = []
keepBups bups i =
  take (keepFromInterval i) $ intervalBups
  where intervalBups    = map head $ groupBy (iPredicate i) $ freeBups
        freeBups        = sortBups $ bups \\ (concatMap (keepBups bups) higherIntervals)
        higherIntervals = drop ((fromJust $ elemIndex i intervals) + 1) intervals


-- Backups to be removed
--
-- This is equivalent to 'all backups that do not belong to one of the
-- intervals'.
toRemove :: [Backup] -> [Backup]
toRemove bups =
  bups \\ (concatMap (keepBups bups) intervals)

-- HELPERS

sortBups :: [Backup] -> [Backup]
sortBups = sortBy (compare `on` snd)

localWeek :: LocalTime -> Int
localWeek = fst . mondayStartWeek . localDay

localYear :: LocalTime -> Integer
localYear = year . toGregorian . localDay
            where year (y,m,d) = y

localHour :: LocalTime -> Int
localHour = todHour . localTimeOfDay

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

timeP :: [(LocalTime -> LocalTime -> Bool)] -> (Backup -> Backup -> Bool)
timeP fs a b = and $ map (\f -> f (bupTime a) (bupTime b)) fs
