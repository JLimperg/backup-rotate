module BackupRotate.Types (
  Backup
, Interval(..)

, bupPath
, bupTime
) where

import Data.Time

-- Backup

type Backup = (FilePath, LocalTime)

bupPath :: Backup -> FilePath
bupPath = fst

bupTime :: Backup -> LocalTime
bupTime = snd

-- Interval

data Interval = Interval { iName      :: String
                         , iPredicate :: (Backup -> Backup -> Bool)
                         , iKeep      :: Int
                         }

instance Eq Interval where
  a == b = iName a == iName b

instance Show Interval where
  show i = '(':(iName i) ++ ' ':(show $ iKeep i) ++ ")"
