module Main where

import Row
import Record
import Record.Classes
import HList
import Data.Monoid

a :: Record '["foo" := String, "bar" := Int]
a = (#foo := "bar") -*- (#bar := 1) -*- Empty

b :: HList '[Int]
b = values #bar a

c :: Record '["foo" := String, "bar" := Int, "foo" := Int]
c = (#foo := "bar") -*- (#bar := 1) -*- (#foo := 1) -*- Empty

d :: Record '["bar" := Int]
d = remove #foo c

e :: Int
e = only #bar c

f :: Record '["foo" := Sum Int, "quux" := String]
f = (#foo := Sum 1) -*- (#quux := "hello") -*- Empty

g :: Record '["bar" := Sum Int, "quux" := String]
g = (#bar := Sum 42) -*- (#quux := ", world") -*- Empty

h = squash $ f `append` g

main :: IO ()
main = print h
