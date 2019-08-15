module HList.Classes where

import HList.Types

-- Homogeneous hlists can be converted to simple lists
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

-- HLists are showable
instance Show (HList '[])
  where
  show _ = "[]"

instance (Show v, Show (HList vs)) => Show (HList (v : vs))
  where
  show (HCons v vs) = show v <> " : " <> show vs
