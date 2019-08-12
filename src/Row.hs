module Row where

import GHC.TypeLits

-- Field value assignments
data Assign k v where
  (:=) :: k -> v -> Assign k v

-- A row is a list of assignments
type Row k v = [Assign k v]
