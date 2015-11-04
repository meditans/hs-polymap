{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
  Module      : Data.PolyMap.Storage.List
  Copyright   : (c) 2015 David Farrell
  License     : PublicDomain
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Storage instance for lists.
-}

module Data.PolyMap.Storage.List
(
) where

import Data.PolyMap.Storage
import Data.List (elemIndices)

instance Storage [] a where
    singleton x = [x]
    lookupIndices = elemIndices
    lookupElem i xs
        | i < 0     = Nothing
        | otherwise = f i xs
      where f _ []     = Nothing
            f 0 (x:_)  = Just x
            f i (_:xs) = f (i - 1) xs
