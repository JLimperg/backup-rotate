-- Copyright (c) 2013 Jannis Limperg
-- License: MIT. See the LICENSE file.

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

intervals = [ Interval {iName = "hourly", iPredicate = hourlyP, iKeep = 12}
            , Interval {iName = "daily" , iPredicate = dailyP , iKeep = 7 }
            , Interval {iName = "weekly", iPredicate = weeklyP, iKeep = 4 }
            ]

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
                         , iKeep  :: Int
                         }

instance Eq Interval where
  a == b = iName a == iName b

instance Show Interval where
  show i = '(':(iName i) ++ ' ':(show $ iKeep i) ++ ")"


--------------------------------------------------------------------------------
-- MAIN AND I/O

main = do
  setupLog

  logI "Beginning rotation."
  logI $ "Processing directory '" ++ backupRoot ++ "'"

  logI $ "Detecting backups according to basename pattern " ++ backupDirPat
  bups' <- Dir.getDirectoryContents backupRoot
  logI $ "Possible backup directories: " ++ show bups'
  bups  <- mapM dateForBackup $ backupDirs . sort $ bups'
  logI $ "Detected backups: " ++ show bups

  let msg i = "Detected " ++ iName i ++ " backups: " ++ show (keepBups bups i)
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
mvall paths = do mapM_ (uncurry mv) paths
                 return ()


remove :: [Backup] -> IO ()
remove []     = return ()
remove (x:xs) = do rmR path
                   remove xs
                where path = temp . expand . bupPath $ x

dateForBackup :: FilePath -> IO Backup
dateForBackup dir = do
  date <- readFile $ expand dir </> "date"
  return (dir, strToDate date)

--------------------------------------------------------------------------------
-- FUNCTIONS

-- I/O-related

backupDirPat :: String
backupDirPat = "^(" ++ iNames ++ ")\\.[0-9]+$"
               where iNames = intercalate "|" $ map iName intervals

backupDirs :: [FilePath] -> [FilePath]
backupDirs xs = filter (regexp backupDirPat) (map (last . splitPath) xs)

strToDate :: String -> LocalTime
strToDate s =
  fromJust (parseTime defaultTimeLocale dateFormat s :: Maybe LocalTime)

enumerate :: [Backup] -> [(FilePath, FilePath)]
enumerate []    = []
enumerate bups  = concatMap (\i -> enumerateInterval (iName i) (iBups i)) intervals
                  where iBups  = sortBups . keepBups bups

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
-- For an explanation of the algorithm, see the README.
keepBups :: [Backup] -> Interval -> [Backup]
keepBups []   _ = []
keepBups bups i =
  take (iKeep i) intervalBups
  where intervalBups    = map head $ groupBy (iPredicate i) freeBups
        freeBups        = sortBups $ bups \\ concatMap (keepBups bups) higherIntervals
        higherIntervals = drop ((fromJust $ elemIndex i intervals) + 1) intervals


-- Backups to be removed
--
-- This is equivalent to 'all backups that do not belong to one of the
-- intervals'.
toRemove :: [Backup] -> [Backup]
toRemove bups =
  bups \\ concatMap (keepBups bups) intervals

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
regexp pat str = BS.pack str =~ BS.pack pat :: Bool

mv :: FilePath -> FilePath -> IO ()
mv src dest = do logI $ "mv '" ++ src ++ "' '" ++ dest ++ "'"
                 -- renameDirectory src dest
                 return ()

rmR :: FilePath -> IO ()
rmR dir = do logI $ "rm -r '" ++ dir ++ "'"
              -- removeDirectory dir
              return ()

temp :: FilePath -> FilePath
temp path = path ++ ".tmp"

expand :: FilePath -> FilePath
expand src = backupRoot </> src

timeP :: [(LocalTime -> LocalTime -> Bool)] -> (Backup -> Backup -> Bool)
timeP fs a b = all (\f -> f (bupTime a) (bupTime b)) fs
