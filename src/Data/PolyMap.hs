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
, PolyMap
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

import Data.PolyMap.Nat
import Data.PolyMap.Relation
import Data.PolyMap.Storage

listElemAt :: Int -> [a] -> Maybe a
listElemAt i xs
    | i < 0     = Nothing
    | otherwise = f i xs
  where f _ []     = Nothing
        f 0 (x:_)  = Just x
        f i (_:xs) = f (i - 1) xs

listLookupIndex :: Eq a => a -> [a] -> Maybe Int
listLookupIndex k xs = f 0 xs
  where f _ [] = Nothing
        f i (x:xs)
            | x == k    = Just i
            | otherwise = f (i + 1) xs

type family HasType a (as :: [*]) :: Bool where
    HasType a '[] = 'False
    HasType a (a ': as) = 'True
    HasType a (b ': as) = HasType a as

type family TypeAt (n :: Nat) (as :: [*]) where
    TypeAt 'Z (a ': as) = a
    TypeAt ('S n) (a ': as) = TypeAt n as

type family MapFst (as :: [(*, *)]) :: [*] where
    MapFst '[] = '[]
    MapFst ('(a, b) ': as) = a ': MapFst as

data family PolyMap (as :: [(*, *)]) :: *
data instance PolyMap '[] = UnitPolyMap
data instance PolyMap ('(a, b) ': as) = [a] :<=>: PolyMap as

infixr 4 :<=>:

deriving instance Show (PolyMap '[])
deriving instance (Show a, Show (PolyMap as)) => Show (PolyMap ('(a, b) ': as))

class PolyMapClass (as :: [(*, *)]) where
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

instance PolyMapClass as => PolyMapClass ('(a, b) ': as) where
    null (xs :<=>: _) = Prelude.null xs
    size (xs :<=>: _) = length xs
    empty = [] :<=>: empty
    singleton' (x :<->: xs) = [x] :<=>: singleton' xs
    insert' (x :<->: xs) (m :<=>: ms) = m ++ [x]:<=>: insert' xs ms
    relationAt i (m :<=>: ms) = (:<->:) <$> listElemAt i m <*> relationAt i ms

class PolyMapLookup (n :: Nat) (as :: [(*, *)]) where
    member :: Proxy n -> TypeAt n (MapFst as) -> PolyMap as -> Bool
    lookupIndex :: Proxy n -> TypeAt n (MapFst as) -> PolyMap as -> Maybe Int

instance PolyMapLookup n '[] where
    member Proxy _ UnitPolyMap = False
    lookupIndex Proxy _ UnitPolyMap = Nothing

instance Eq a => PolyMapLookup 'Z ('(a, b) ': as) where
    member Proxy x (xs :<=>: _) = elem x xs
    lookupIndex Proxy x (xs :<=>: _) = listLookupIndex x xs

instance (PolyMapLookup n as) => PolyMapLookup ('S n) ('(a, b) ': as) where
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
