{-# LANGUAGE Safe #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
  Module      : Data.PolyMap
  Copyright   : (c) 2015 David Farrell
  License     : PublicDomain
  Stability   : unstable
  Portability : portable

  Polydirectional maps.
-}

module Data.PolyMap
( PolyMap
, Relation(..)
, empty
, singleton
, insert
) where

data family PolyMap :: [*] -> *
data instance PolyMap '[] = UnitPolyMap
data instance PolyMap (a ': as) = [a] :<=>: PolyMap as

infixr 4 :<=>:

deriving instance Show (PolyMap '[])
deriving instance (Show a, Show (PolyMap as)) => Show (PolyMap (a ': as))

data family Relation :: [*] -> *
data instance Relation '[] = UnitRelation
data instance Relation (a ': as) = a :<->: Relation as

infixr 4 :<->:

deriving instance Show (Relation '[])
deriving instance (Show a, Show (Relation as)) => Show (Relation (a ': as))

class PolyMapClass (as :: [*]) where
    null :: PolyMap as -> Bool
    size :: PolyMap as -> Int
    empty :: PolyMap as
    singleton :: Relation as -> PolyMap as
    insert :: Relation as -> PolyMap as -> PolyMap as

instance PolyMapClass '[] where
    null UnitPolyMap = True
    size UnitPolyMap = 0
    empty = UnitPolyMap
    singleton UnitRelation = UnitPolyMap
    insert UnitRelation UnitPolyMap = UnitPolyMap

instance PolyMapClass as => PolyMapClass (a ': as) where
    null ([] :<=>: _) = True
    null (_  :<=>: _) = False
    size ([] :<=>: _) = 0
    size (xs :<=>: _) = length xs
    empty = [] :<=>: empty
    singleton (x :<->: xs) = [x] :<=>: singleton xs
    insert (x :<->: xs) (m :<=>: ms) = x:m :<=>: insert xs ms
