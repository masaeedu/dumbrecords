module HList.Types where

data HList xs
  where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

(-:-) = HCons
infixr -:-
