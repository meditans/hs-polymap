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
    -- TODO: use this when containers (hopefully) gets it: lookupElem i s = S.lookupElem i s
    lookupElem i s
        | i >= 0 && i < S.size s = Just (S.elemAt i s)
        | otherwise              = Nothing
