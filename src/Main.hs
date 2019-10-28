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

f :: Record '["foo" := Int, "quux" := String]
f = (#foo := 1) -*- (#quux := "hello") -*- Empty

g :: Record '["bar" := Int, "quux" := String]
g = (#bar := 42) -*- (#quux := ", world") -*- Empty

h :: Record '["foo" := Int, "quux" := String, "bar" := Int, "quux" := String]
h = f `append` g

i :: HList '[String, String]
i = values #quux h

j :: Record '["foo" := Int, "bar" := Int]
j = remove #quux h

k :: Record '["quux" := String, "quux" := String]
k = (#quux := "Hello, ") -*- (#quux := "World") -*- Empty

l :: Record '["quux" := String]
l = squash k

m :: Record '["foo" := Int, "quux" := String, "bar" := Int]
m = f `squishyAppend` g

main :: IO ()
main = print h
