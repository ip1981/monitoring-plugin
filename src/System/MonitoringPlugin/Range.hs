{-|
Defines threshold range according to <https://nagios-plugins.org/doc/guidelines.html>.

 >>> import System.MonitoringPlugin.Range
 >>> let r1 = read "50:100" :: Range Int
 >>> let r2 = read "1" :: Range Double
 >>> let r3 = read "@-1:1" :: Range Double
 >>> let r4 = read "~:" :: Range Integer
 >>> 40 `within` r1
False
 >>> 0.5 `within` r2
True
 >>> 0.5 `within` r3
False
 >>> (-9999999999999999999) `within` r4
True
 >>> read "0:100" :: Range Int
100
 >>> read ":100" :: Range Int
*** Exception: Prelude.read: no parse
 >>> read "~:100" :: Range Int
~:100
 >>> read "2:1" :: Range Double
*** Exception: Prelude.read: no parse
-}
module System.MonitoringPlugin.Range
  ( Range
  , within
  ) where

-- | The range. The only way to contruct it is to 'read' a 'String'.
data Range n
  = Inner (Value n)
          (Value n)
  | Outer (Value n)
          (Value n)

-- | Test if a value belongs to the range.
within :: Ord n => n -> Range n -> Bool
within p (Inner v1 v2) = v1 <= v && v <= v2
  where
    v = Fin p
within p (Outer v1 v2) = v < v1 || v2 < v
  where
    v = Fin p

instance (Num n, Ord n, Show n) => Show (Range n) where
  show = showRange

showRange :: (Num n, Ord n, Show n) => Range n -> String
showRange (Outer v1 v2) = '@' : showRange (Inner v1 v2)
showRange (Inner v1 v2) =
  if Fin 0 /= v1
    then show v1 ++ ":" ++ show v2
    else show v2

instance (Num n, Ord n, Read n) => Read (Range n) where
  readsPrec _ [] = []
  readsPrec _ s = [(r, "") | v1 <= v2]
    where
      outter = head s == '@'
      r =
        if outter
          then Outer v1 v2
          else Inner v1 v2
      s' =
        if outter
          then tail s
          else s
      (s1, s2) = span (/= ':') s'
      (v1, v2) =
        if null s2
          then (Fin 0, read s1)
          else (read s1, read $ tail s2)

data Value n
  = NInf
  | Fin n
  | PInf

instance (Show n) => Show (Value n) where
  show NInf = "~"
  show PInf = ""
  show (Fin a) = show a

instance (Read n) => Read (Value n) where
  readsPrec _ [] = [(PInf, "")]
  readsPrec _ s
    | head s == '~' = [(NInf, tail s)]
    | otherwise = [(Fin $ read s, "")]

instance (Eq n) => Eq (Value n) where
  Fin a == Fin b = a == b
  NInf == NInf = True
  PInf == PInf = True
  _ == _ = False

instance (Ord n) => Ord (Value n) where
  compare (Fin a) (Fin b) = compare a b
  compare NInf NInf = EQ
  compare PInf PInf = EQ
  compare NInf _ = LT
  compare PInf _ = GT
  compare _ NInf = GT
  compare _ PInf = LT
