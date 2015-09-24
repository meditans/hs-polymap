{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
  Module      : Data.PolyMap.Storage.List
  Copyright   : (c) 2015 David Farrell
  License     : PublicDomain
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Storage instance for [].
-}

module Data.PolyMap.Storage.List where

import Data.PolyMap.Storage

instance Storage [] a where
    singleton x = [x]
    lookupIndex k xs = f 0 xs
      where f _ [] = Nothing
            f i (x:xs)
                | x == k    = Just i
                | otherwise = f (i + 1) xs
    lookupElem i xs
        | i < 0     = Nothing
        | otherwise = f i xs
      where f _ []     = Nothing
            f 0 (x:_)  = Just x
            f i (_:xs) = f (i - 1) xs
