module Main where

import Record

-- A simple test
rec1 :: RecordS $ ("foo" -: Int) $ ("quux" -: String) $ Empty
rec1 = (#foo -: 1) $ (#quux -: "hello") $ EmptyS

rec2 :: RecordS $ ("bar" -: Int) $ ("quark" -: String) $ Empty
rec2 = (#bar -: 42) $ (#quark -: ", world") $ EmptyS

getfoo :: Get "foo" Int o => RecordS o -> Int
getfoo = get #foo

bar :: Int
bar = getfoo rec1

rec3 :: RecordS $ ("foo" -: Int) $ ("quux" -: String) $ ("bar" -: Int) $ ("quark" -: String) $ Empty
rec3 = rec1 `append` rec2

quux :: String
quux = get #quux rec3

main :: IO ()
main = print rec3
