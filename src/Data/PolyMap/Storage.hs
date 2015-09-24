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

class (Monoid (s a), Foldable s) => Storage s a where
    singleton :: a -> s a
    lookupIndex :: Eq a => a -> s a -> Maybe Int
    lookupElem :: Int -> s a -> Maybe a
