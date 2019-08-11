module Record where

import GHC.TypeLits
import Data.Proxy
import Data.Type.Equality

data Row k v
  where
  Empty :: Row k v
  With  :: k -> v -> Row k v -> Row k v

data RecordS (r :: Row Symbol *)
  where
  EmptyS :: RecordS Empty
  WithS  :: Proxy k -> v -> RecordS o -> RecordS (With k v o)

-- Some thinguses to make the syntax nice
fromLabel :: forall k. Proxy k
fromLabel = Proxy

type f $ a = f a
infixr $

type (-:) = With

(-:) :: Proxy k -> v -> RecordS o -> RecordS (With k v o)
(p -: v) o = WithS p v o

-- Class representing whether a record lacks a field
class Lacks k o

-- An empty record lacks any key
instance
  Lacks k Empty

-- A record where the head doesn't match the key
-- lacks the key if the rest of the record lacks it
instance ((k == l) ~ False, Lacks k o) =>
  Lacks k (With l v o)

-- Class for retrieving a particular field from a record
class Get k v o
  where
  get :: Proxy k -> RecordS o -> v

-- A record that has the field at the head (and nowhere else)
-- has the key
instance
  Get k v (With k v o)
  where
  get _ (WithS _ v _) = v

-- A record that doesn't have the field at the head, but does
-- have it somewhere else, has the key
instance ((k == l) ~ False, Get k v o) =>
  Get k v (With l x o)
  where
  get k (WithS _ _ o) = get k o

class Remove k o o' where
  remove :: Proxy k -> RecordS o -> RecordS o'

instance
  Remove k (With k v o) o
  where
  remove _ (WithS _ _ o) = o

instance ((k == l) ~ False, Remove k o o') =>
  Remove k (With l v o) (With l v o')
  where
  remove k (WithS l v o) = WithS l v (remove k o)

-- Appending two records
class Append r1 r2 r3 where
  append :: RecordS r1 -> RecordS r2 -> RecordS r3

-- An empty record can be appended to any other record
instance Append Empty r r
  where
  append _ x = x

-- Also works the other way
instance Append r Empty r
  where
  append x _ = x

-- A record can be appended onto another record that lacks
-- a particular key
instance (Lacks k y, Append x y z) =>
  Append (With k v x) y (With k v z)
  where
  append (WithS k v x) y = (k -: v) $ (append x y)

-- A record can be appended onto another record that shares
-- a key, whenever there is a semigroup instance for the shared
-- field
-- instance (Semigroup v, Get k v y, Remove k y y', (x <> y') z) =>
--   (With k v x <> y) (With k v z)
--   where
--   append (WithS k v x) y = WithS k (v <> get k y) (append x (remove k y :: RecordS y'))

-- Squash duplicates using a monoid
-- class Squash r r' where
--   squash :: RecordS r -> RecordS r'
--
-- instance
--   Squash Empty Empty
--   where
--   squash = id
--
-- instance (Semigroup v, Get k v o, Remove k o o') =>
--   Squash (With k v o)
--   where

class ShowR r where
  showR :: RecordS r -> String

instance
  ShowR Empty
  where
  showR EmptyS = "{}"

instance (KnownSymbol k, Show v, ShowR o) =>
  ShowR (With k v o)
  where
  showR (WithS k v o) = "{ " ++ (symbolVal k) ++ " : " ++ (show v) ++ " } + " ++ (showR o)

instance ShowR r =>
  Show (RecordS r)
  where
  show = showR
