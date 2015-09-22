{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
  Module      : Data.PolyMap
  Copyright   : (c) 2015 David Farrell
  License     : PublicDomain
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Polydirectional maps.
-}

module Data.PolyMap
( Relation(..)
, PolyMap
, Data.PolyMap.null
, size
, member
, notMember
, empty
, singleton
, insert
) where

import Data.PolyMap.Nat

-- HasType

type family HasType a (as :: [*]) :: Bool where
    HasType a '[] = 'False
    HasType a (a ': as) = 'True
    HasType a (b ': as) = HasType a as

-- TypeAt

type family TypeAt (n :: Nat) (as :: [*]) where
    TypeAt 'Z (a ': as) = a
    TypeAt ('S n) (a ': as) = TypeAt n as

-- Relation

data family Relation (as :: [*])
data instance Relation '[] = UnitRelation
data instance Relation (a ': as) = a :<->: Relation as

infixr 4 :<->:

deriving instance Show (Relation '[])
deriving instance (Show a, Show (Relation as)) => Show (Relation (a ': as))

-- PolyMap

data family PolyMap (as :: [*]) :: *
data instance PolyMap '[] = UnitPolyMap
data instance PolyMap (a ': as) = [a] :<=>: PolyMap as

infixr 4 :<=>:

deriving instance Show (PolyMap '[])
deriving instance (Show a, Show (PolyMap as)) => Show (PolyMap (a ': as))

-- PolyMapClass

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
    null (xs :<=>: _) = Prelude.null xs
    size (xs :<=>: _) = length xs
    empty = [] :<=>: empty
    singleton (x :<->: xs) = [x] :<=>: singleton xs
    insert (x :<->: xs) (m :<=>: ms) = x:m :<=>: insert xs ms

class PolyMapLookup (n :: Nat) (as :: [*]) where
    member :: Proxy n -> TypeAt n as -> PolyMap as -> Bool

instance PolyMapLookup n '[] where
    member Proxy _ UnitPolyMap = False

instance Eq a => PolyMapLookup 'Z (a ': as) where
    member Proxy x (xs :<=>: _) = elem x xs

instance (PolyMapLookup n as) => PolyMapLookup ('S n) (a ': as) where
    member Proxy x (_ :<=>: ms) = member (Proxy :: Proxy n) x ms

notMember :: PolyMapLookup n as => Proxy n -> TypeAt n as -> PolyMap as -> Bool
notMember p x m = not (member p x m)
