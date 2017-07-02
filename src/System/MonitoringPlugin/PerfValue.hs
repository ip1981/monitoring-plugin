{-|
Defines performance values according to <https://nagios-plugins.org/doc/guidelines.html>.
This module is meant to be imported qualified.

>>> import System.MonitoringPlugin.Range (Range(..))
>>> import qualified System.MonitoringPlugin.PerfValue as V
>>> let w = read "10:30" :: Range Int
>>> V.PerfValue {V.value = 17, V.unit = V.Second, V.min = Just 0, V.max = Just 600, V.warn = Just w, V.crit = Nothing}
17s;10:30;;0;600
>>> let c = read "0:100" :: Range Double
>>> V.PerfValue {V.value = 12.3, V.unit = V.Terabyte, V.min = Just 3.1, V.max = Nothing, V.warn = Nothing, V.crit = Just c}
12.3TB;;100.0;3.1;

-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module System.MonitoringPlugin.PerfValue
  ( PerfValue(..)
  , Unit(..)
  ) where

import Prelude hiding (max, min)

import System.MonitoringPlugin.Range (Range)

-- | Performance value.
data PerfValue = forall n. (Num n, Ord n, Show n) =>
                           PerfValue
  { value :: n
  , unit :: Unit
  , min :: Maybe n
  , max :: Maybe n
  , warn :: Maybe (Range n)
  , crit :: Maybe (Range n)
  }

instance Show PerfValue where
  show PerfValue {..} =
    show value ++
    show unit ++
    ";" ++
    maybe "" show warn ++
    ";" ++
    maybe "" show crit ++ ";" ++ maybe "" show min ++ ";" ++ maybe "" show max

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
