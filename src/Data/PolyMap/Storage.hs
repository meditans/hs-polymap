{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
  Module      : Data.PolyMap.Storage
  Copyright   : (c) 2015 David Farrell
  License     : PublicDomain
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Storage typeclass for polymaps.
-}

module Data.PolyMap.Storage
( Storage
, singleton
, lookupIndex
, lookupElem
) where

import qualified Data.Set as S

class (Monoid (s a), Foldable s) => Storage s a where
    singleton :: a -> s a
    lookupIndex :: Eq a => a -> s a -> Maybe Int
    lookupElem :: Int -> s a -> Maybe a

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

instance Ord a => Storage S.Set a where
    singleton x = S.singleton x
    lookupIndex k s = S.lookupIndex k s
    lookupElem i s = S.lookupElem i s
