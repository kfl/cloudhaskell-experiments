{-
  Round-Trip Latency Benchmark
  Course: 7,5 ECTS project
  Date: 30/3/12
  
  Brian Avlund
-}

{-# LANGUAGE DeriveDataTypeable,TemplateHaskell, TypeSynonymInstances #-}
module Structs where

import Remote
--import Remote.Call (mkClosure)
import Data.Binary (Binary,get,put,encode,decode)

{- ------------------ Flist ------------------ -}
type FList = Maybe Int -> [String]

instance Binary FList where 
                put = genericPut
                get = genericGet

empty :: FList
empty = \ _ -> []

add :: String -> FList -> FList
add x fl = \ pre ->
  case pre of
    Nothing        -> x : fl Nothing
    Just n | n > 0 -> x : (fl $ Just $ n-1)
    _              -> []
        
fromList :: [String] -> FList
fromList xs = foldr add empty xs

toList :: FList -> [String]
toList fl = fl Nothing

merge :: FList -> FList -> FList
merge xs ys = foldr add ys $ toList xs

ftake :: Int -> FList -> [String]
ftake n fl = fl $ Just n

{- ------------------ Simple list ------------------ -}
type SList = [String]

emptyS :: SList
emptyS = []

addS :: String -> SList -> SList
addS x l = x : l

fromListS :: [String] -> SList
fromListS xs = xs -- foldr addS empty xs

toListS :: [String] -> [String]
toListS l = l

mergeS :: SList -> SList -> SList
mergeS xs ys = foldr addS ys $ toListS xs

stake :: Int -> SList -> [String]
stake n fl = take n fl

{- ------------------ Simple int ------------------ -}
type SInt = Int

emptySi :: SInt
emptySi = 0

addSi :: Int -> SInt -> SInt
addSi x l = x + l

fromListSi :: Int -> SInt
fromListSi xs = xs -- foldr addS empty xs

toListSi :: SInt -> Int
toListSi l = l

mergeSi :: SInt -> SInt -> SInt
mergeSi xs ys = addSi ys xs