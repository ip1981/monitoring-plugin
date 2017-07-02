{-|
Defines performance data according to <https://nagios-plugins.org/doc/guidelines.html>.

 >>> import System.MonitoringPlugin.Range
 >>> import System.MonitoringPlugin.PerfData
 >>> let w = read "10:30" :: Range Int
 >>> PerfValue {pValue = 17, pUnit = Second, pMin = Just 0, pMax = Just 600, pWarn = Just w, pCrit = Nothing}
17s;10:30;;0;600
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module System.MonitoringPlugin.PerfData
  ( PerfData(..)
  , PerfValue(..)
  , Unit(..)
  ) where

import Data.HashMap.Strict (HashMap)

import System.MonitoringPlugin.Range (Range)

-- | Performance data.
data PerfData =
  PerfData (HashMap String PerfValue)

instance Show PerfData where
  show hm = "hello"

-- | Performance value.
data PerfValue = forall n. (Num n, Ord n, Show n) =>
                           PerfValue
  { pValue :: n
  , pUnit :: Unit
  , pMin :: Maybe n
  , pMax :: Maybe n
  , pWarn :: Maybe (Range n)
  , pCrit :: Maybe (Range n)
  }

instance Show PerfValue where
  show PerfValue {..} =
    show pValue ++
    show pUnit ++
    ";" ++
    maybe "" show pWarn ++
    ";" ++
    maybe "" show pCrit ++
    ";" ++ maybe "" show pMin ++ ";" ++ maybe "" show pMax

-- | Unit of measure.
data Unit
  = Byte
  | Counter
  | Gigabyte
  | Kilobyte
  | Megabyte
  | Microsecond
  | Millisecond
  | None
  | Percent
  | Second
  | Terabyte

instance Show Unit where
  show Byte = "B"
  show Counter = "c"
  show Gigabyte = "GB"
  show Kilobyte = "KB"
  show Megabyte = "MB"
  show Microsecond = "ms"
  show Millisecond = "us"
  show None = ""
  show Percent = "%"
  show Second = "s"
  show Terabyte = "TB"
