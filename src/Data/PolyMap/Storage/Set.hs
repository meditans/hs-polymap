{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
  Module      : Data.PolyMap.Storage.Set
  Copyright   : (c) 2015 David Farrell
  License     : PublicDomain
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Storage instance for Data.Set.
-}

module Data.PolyMap.Storage.Set
( S.Set
) where

import qualified Data.Set as S
import Data.PolyMap.Storage

instance Ord a => Storage S.Set a where
    singleton x = S.singleton x
    lookupIndex k s = S.lookupIndex k s
    lookupElem i s = S.lookupElem i s
