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
, lookupIndices
, lookupElem
) where

-- |Typeclass for data types that can act as storage in a polymap.
class (Monoid (s a), Foldable s) => Storage s a where
    singleton :: a -> s a
    lookupIndices :: Eq a => a -> s a -> [Int]
    lookupElem :: Int -> s a -> Maybe a
