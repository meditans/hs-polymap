{-# LANGUAGE Safe #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
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
( module Data.PolyMap.Relation
, Set
, PolyMap
, SimplePolyMap
, Data.PolyMap.null
, size
, member
, notMember
, Data.PolyMap.lookup
, empty
, singleton
, insert
, lookupIndex
, relationAt
) where

import Data.Set (Set)
import Data.PolyMap.Nat
import Data.PolyMap.Relation
import Data.PolyMap.Storage (Storage)
import qualified Data.PolyMap.Storage as S

type family HasType a (as :: [*]) :: Bool where
    HasType a '[] = 'False
    HasType a (a ': as) = 'True
    HasType a (b ': as) = HasType a as

type family TypeAt (n :: Nat) (as :: [*]) where
    TypeAt 'Z (a ': as) = a
    TypeAt ('S n) (a ': as) = TypeAt n as

type family MapStorage (f :: kf) (as :: [k0]) :: [(k0, kf)] where
    MapStorage f '[] = '[]
    MapStorage f (a ': as) = '(a, f) ': MapStorage f as

type family MapFst (as :: [(k0, k1)]) :: [k0] where
    MapFst '[] = '[]
    MapFst ('(a, b) ': as) = a ': MapFst as

data family PolyMap (as :: [(*, * -> *)])
data instance PolyMap '[] = UnitPolyMap
data instance PolyMap ('(a, s) ': as) = s a :<=>: PolyMap as

type SimplePolyMap (as :: [*]) (f :: * -> *) = PolyMap (MapStorage f as)

infixr 4 :<=>:

deriving instance Show (PolyMap '[])
deriving instance (Show a, Show (s a), Show (PolyMap as)) => Show (PolyMap ('(a, s) ': as))

class PolyMapClass (as :: [(*, * -> *)]) where
    null :: PolyMap as -> Bool
    size :: PolyMap as -> Int
    empty :: PolyMap as
    singleton' :: Relation (MapFst as) -> PolyMap as
    insert' :: Relation (MapFst as) -> PolyMap as -> PolyMap as
    relationAt :: Int -> PolyMap as -> Maybe (Relation (MapFst as))

instance PolyMapClass '[] where
    null UnitPolyMap = True
    size UnitPolyMap = 0
    empty = UnitPolyMap
    singleton' UnitRelation = UnitPolyMap
    insert' UnitRelation UnitPolyMap = UnitPolyMap
    relationAt _ UnitPolyMap = Just UnitRelation

instance (Storage s a, PolyMapClass as) => PolyMapClass ('(a, s) ': as) where
    null (xs :<=>: _) = Prelude.null xs
    size (xs :<=>: _) = length xs
    empty = mempty :<=>: empty
    singleton' (x :<->: xs) = S.singleton x :<=>: singleton' xs
    insert' (x :<->: xs) (m :<=>: ms) = mconcat [m, (S.singleton x)] :<=>: insert' xs ms
    relationAt i (m :<=>: ms) = (:<->:) <$> S.lookupElem i m <*> relationAt i ms

class PolyMapLookup (n :: Nat) (as :: [(*, * -> *)]) where
    member :: Proxy n -> TypeAt n (MapFst as) -> PolyMap as -> Bool
    lookupIndex :: Proxy n -> TypeAt n (MapFst as) -> PolyMap as -> Maybe Int

instance PolyMapLookup n '[] where
    member Proxy _ UnitPolyMap = False
    lookupIndex Proxy _ UnitPolyMap = Nothing

instance (Eq a, Storage s a) => PolyMapLookup 'Z ('(a, s) ': as) where
    member Proxy x (xs :<=>: _) = elem x xs
    lookupIndex Proxy x (xs :<=>: _) = S.lookupIndex x xs

instance (Storage s a, PolyMapLookup n as) => PolyMapLookup ('S n) ('(a, s) ': as) where
    member Proxy x (_ :<=>: ms) = member (Proxy :: Proxy n) x ms
    lookupIndex Proxy x (_ :<=>: ms) = lookupIndex (Proxy :: Proxy n) x ms

notMember :: PolyMapLookup n as => Proxy n -> TypeAt n (MapFst as) -> PolyMap as -> Bool
notMember proxy x m = not (member proxy x m)

lookup :: (PolyMapClass as, PolyMapLookup n as) => Proxy n -> TypeAt n (MapFst as) -> PolyMap as -> Maybe (Relation (MapFst as))
lookup proxy x m = case lookupIndex proxy x m of
    Nothing -> Nothing
    Just i  -> relationAt i m

singleton :: (PolyMapClass as, ToRelation a (MapFst as)) => a -> PolyMap as
singleton r = singleton' (toRelation r)

insert :: (PolyMapClass as, ToRelation a (MapFst as)) => a -> PolyMap as -> PolyMap as
insert r m = insert' (toRelation r) m
