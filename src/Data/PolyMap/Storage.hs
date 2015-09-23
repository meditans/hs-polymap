{-# LANGUAGE Safe #-}

{-|
  Module      : Data.PolyMap.Storage
  Copyright   : (c) 2015 David Farrell
  License     : PublicDomain
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Storage typeclass for polymaps.
-}

module Data.PolyMap.Storage where

class Storage a where
    storage :: a -> Bool

instance Storage [a] where
    storage _ = True
