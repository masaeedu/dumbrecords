{-# LANGUAGE FunctionalDependencies #-}

module Record.Classes where

import GHC.TypeLits
import Data.Proxy
import Data.Type.Equality
import Data.Foldable

import Row
import HList
import Record.Types

-- Class representing whether a record lacks a field
class Lacks k (o :: Row Symbol *)

-- An empty record lacks any key
instance
  Lacks k '[]

-- A record where the head doesn't match the key
-- lacks the key if the rest of the record lacks it
instance ((k == l) ~ False, Lacks k o) =>
  Lacks k ((l := v) : o)

-- Get all occurrences of a field in a record
class Values k o vs | k o -> vs
  where
  values :: Proxy k -> Record o -> HList vs

instance
  Values k '[] '[]
  where
  values _ _ = HNil

instance Values k r vs =>
  Values k ((k := v) : r) (v : vs)
  where
  values k (With (_ := v) r) = HCons v (values k r)

instance ((k == l) ~ False, Values k r vs) =>
  Values k ((l := x) : r) vs
  where
  values k (With _ r) = values k r

-- Get the only occurrence of a field in a record
only :: forall k v r. Values k r '[v] => Proxy k -> Record r -> v
only k r = case (values k r) of
  (HCons v HNil) -> v

-- Remove a field from a record
class Remove k r r' | k r -> r'
  where
  remove :: Proxy k -> Record r -> Record r'

instance (Lacks k r) =>
  Remove k r r
  where
  remove _ r = r

instance (Remove k r r') =>
  Remove k ((k := v) : r) r'
  where
  remove k (With _ r) = (remove k r)

instance ((k == l) ~ False, Remove k r r') =>
  Remove k ((l := v) : r) ((l := v) : r')
  where
  remove k (With a r) = With a (remove k r)

-- Append two records
class Append r1 r2 r3 | r1 r2 -> r3
  where
  append :: Record r1 -> Record r2 -> Record r3

instance
  Append '[] r r
  where
  append _ r = r

instance
  Append r '[] r
  where
  append r _ = r

instance Append r s t =>
  Append (a : r) s (a : t)
  where
  append (With a r) s = With a (append r s)

-- Deduplicate a record using a monoid
class Squash r s | r -> s
  where
  squash :: Record r -> Record s

instance
  Squash '[] '[]
  where
  squash = id

instance (Monoid v, Values k r vs, Homogeneous vs v, Remove k r r') =>
  Squash ((k := v) : r) ((k := v) : r')
  where
  squash (With (k := v) r) = With (k := (v <> fold (HList.toList (values k r)))) (remove k r)

-- Records are showable
class ShowR r where
  showR :: Record r -> String

instance
  ShowR '[]
  where
  showR Empty = "{}"

instance (KnownSymbol k, Show v, ShowR r) =>
  ShowR ((k := v) : r)
  where
  showR (With (k := v) o) = "{ " ++ (symbolVal k) ++ " : " ++ (show v) ++ " } + " ++ (showR o)

instance ShowR r =>
  Show (Record r)
  where
  show = showR