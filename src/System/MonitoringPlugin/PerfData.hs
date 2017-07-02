{-|
Defines performance data according to <https://nagios-plugins.org/doc/guidelines.html>.
-}
module System.MonitoringPlugin.PerfData
  ( PerfData(..)
  ) where

import Data.HashMap.Strict (HashMap)

import System.MonitoringPlugin.PerfValue (PerfValue)

-- | Performance data.
data PerfData =
  PerfData (HashMap String PerfValue)

instance Show PerfData where
  show hm = "hello"
