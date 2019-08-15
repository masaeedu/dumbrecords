{-# LANGUAGE FunctionalDependencies #-}

module Record.Classes where

import GHC.TypeLits
import Data.Proxy
import Data.Type.Equality
import Data.Foldable
import Data.Bool
import Data.Char

import Row
import HList
import Record.Types

-- Class representing whether a record lacks a field
class Lacks k r b | k r -> b

-- An empty record lacks any key
instance
  Lacks k '[] True

class LacksCase b k l v r o | b k l v r -> o

instance
  LacksCase True k l v r False

instance Lacks k r b =>
  LacksCase False k l v r b

-- A record where the head doesn't match the key
-- lacks the key if the rest of the record lacks it
instance ((k == l) ~ b, LacksCase b k l v r o) =>
  Lacks k ((l := v) : r) o

-- Get all occurrences of a field in a record
class Get k r vs | k r -> vs
  where
  values :: Proxy k -> Record r -> HList vs

instance
  Get k '[] '[]
  where
  values _ _ = HNil

class GetCase b k l v r vs | b k l v r -> vs
  where
  values' :: Proxy k -> Proxy l -> v -> Record r -> HList vs

instance Get k r vs =>
  GetCase False k l v r vs
  where
  values' k l v r = values k r

instance Get k r vs =>
  GetCase True k l v r (v : vs)
  where
  values' k l v r = HCons v (values k r)

instance ((k == l) ~ b, GetCase b k l v r vs) =>
  Get k ((l := v) : r) vs
  where
  values k (With (l := v) r) = values' @b k l v r

-- Get the only occurrence of a field in a record
only :: forall k v r. Get k r '[v] => Proxy k -> Record r -> v
only k r = case (values k r) of
  (HCons v HNil) -> v

-- Remove a field from a record
class Remove k r r' | k r -> r'
  where
  remove :: Proxy k -> Record r -> Record r'

instance
  Remove k '[] '[]
  where
  remove _ = id

class RemoveCase b k l v r r' | b k l v r -> r'
  where
  remove' :: Proxy k -> Proxy l -> v -> Record r -> Record r'

instance Remove k r r' =>
  RemoveCase True k l v r r'
  where
  remove' k l v r = remove k r

instance Remove k r r' =>
  RemoveCase False k l v r ((l := v) : r')
  where
  remove' k l v r = With (l := v) (remove k r)

instance ((k == l) ~ b, RemoveCase b k l v r r') =>
  Remove k ((l := v) : r) r'
  where
  remove k (With (l := v) r) = remove' @b k l v r

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

-- Records are showable
class ShowR r where
  showR :: Record r -> String

instance
  ShowR '[]
  where
  showR Empty = "{  }"

instance (KnownSymbol k, Show v, ShowR r) =>
  ShowR ((k := v) : r)
  where
  -- TODO: Work out a nicer way to do this
  showR (With (k := v) o) = "{ \"" <> symbolVal k <> "\": " <> show v <> rest <> " }"
    where
    rest =
      let r = init $ init $ drop 2 $ showR o
      in bool (", " <> r) "" (null $ dropWhile isSpace $ r)

instance ShowR r =>
  Show (Record r)
  where
  show = showR

-- Deduplicate a record using monoid instances for all the duplicated fields
class Squash r r' | r -> r'
  where
  squash :: Record r -> Record r'

instance
  Squash '[] '[]
  where
  squash = id

class SquashCase b k v r s | b k v r -> s
  where
  squash' :: Proxy k -> v -> Record r -> Record s

instance Squash r s =>
  SquashCase True k v r ((k := v) : s)
  where
  squash' k v r = With (k := v) (squash r)

instance
  ( Monoid v
  , Get k r vr
  , Homogeneous vr v
  , Remove k r s
  , Squash s t
  ) =>
  SquashCase False k v r ((k := v) : t)
  where
  squash' k v r = With (k := v') r'
    where
    v' = v <> (fold $ HList.toList $ values k $ r)
    r' = squash $ remove k $ r

instance (Lacks k r b, SquashCase b k v r s) =>
  Squash ((k := v) : r) s
  where
  squash (With (k := v) r) = squash' @b k v r
