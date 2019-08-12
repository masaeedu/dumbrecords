module HList.Classes where

import HList.Types

class Homogeneous vs v
  where
  toList :: HList vs -> [v]

instance
  Homogeneous '[] v
  where
  toList _ = []

instance (Homogeneous vs v) =>
  Homogeneous (v : vs) v
  where
  toList (HCons v vs) = v : (toList vs)
