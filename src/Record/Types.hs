module Record.Types where

import GHC.TypeLits
import Data.Proxy

import Row

-- A record is either empty, or a key + value + record
data Record (r :: Row Symbol *)
  where
  Empty :: Record '[]
  With  :: (Assign (Proxy k) v) -> Record r -> Record ((k := v) : r)

-- Some things to make the syntax nice
fromLabel :: Proxy k
fromLabel = Proxy

(-*-) = With
infixr -*-
