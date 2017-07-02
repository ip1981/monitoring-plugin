{-|
Check status.
-}
module System.MonitoringPlugin.CheckStatus
  ( CheckStatus(..)
  ) where

-- | Check status (plugin exit code).
data CheckStatus
  = OK -- ^ Everything is good, 0.
  | Warning -- ^ Nothing is broken, but this needs attention, 1.
  | Critical -- ^ Emergency, 2.
  | Unknown -- ^ Status is unknown, 3.
  deriving (Enum, Eq, Ord)

instance Show CheckStatus where
  show OK = "OK"
  show Warning = "WARNING"
  show Critical = "CRITICAL"
  show Unknown = "UNKNOWN"
