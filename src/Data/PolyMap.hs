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

  Polydirectional maps for Haskell with flexible storage.
-}

module Data.PolyMap
( module Data.PolyMap.Relation
, module Data.PolyMap.Storage.Set

-- * PolyMap Type
, PolyMap
, SimplePolyMap

-- * Query
, Data.PolyMap.null
, size
, member
, notMember
, Data.PolyMap.lookup

-- * Construction
, empty
, singleton

-- * Insertion
, insert

-- * Indexed
, lookupIndex
, lookupRelation
) where

import Data.PolyMap.Nat
import Data.PolyMap.Relation
import Data.PolyMap.Storage (Storage)
import qualified Data.PolyMap.Storage as S
import Data.PolyMap.Storage.List()
import Data.PolyMap.Storage.Set

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

-- |A polymap whose sides are defined by a list of types zipped with storage types.
data family PolyMap (as :: [(*, * -> *)])
data instance PolyMap '[] = UnitPolyMap
data instance PolyMap ('(a, s) ': as) = s a :<=>: PolyMap as

-- |A simple polymap whose sides are defined by a list of types and a single storage type.
type SimplePolyMap (as :: [*]) (s :: * -> *) = PolyMap (MapStorage s as)

infixr 4 :<=>:

deriving instance Show (PolyMap '[])
deriving instance (Show a, Show (s a), Show (PolyMap as)) => Show (PolyMap ('(a, s) ': as))

class PolyMapClass (as :: [(*, * -> *)]) where
    -- |Is the polymap empty?
    null :: PolyMap as -> Bool

    -- |The number of relations in the polymap.
    size :: PolyMap as -> Int

    -- |The empty polymap.
    empty :: PolyMap as

    -- |Retrieve a relation by its /index/, i.e. by the zero-based index of the
    -- storage of each of its sides. The index is a number from /0/ up to, but
    -- not including, the 'size' of the polymap.
    lookupRelation :: Int -> PolyMap as -> Maybe (Relation (MapFst as))

    singleton' :: Relation (MapFst as) -> PolyMap as
    insert' :: Relation (MapFst as) -> PolyMap as -> PolyMap as

instance PolyMapClass '[] where
    null UnitPolyMap = True
    size UnitPolyMap = 0
    empty = UnitPolyMap
    singleton' UnitRelation = UnitPolyMap
    insert' UnitRelation UnitPolyMap = UnitPolyMap
    lookupRelation _ UnitPolyMap = Just UnitRelation

instance (Storage s a, PolyMapClass as) => PolyMapClass ('(a, s) ': as) where
    null (xs :<=>: _) = Prelude.null xs
    size (xs :<=>: _) = length xs
    empty = mempty :<=>: empty
    singleton' (x :<->: xs) = S.singleton x :<=>: singleton' xs
    insert' (x :<->: xs) (m :<=>: ms) = mconcat [m, (S.singleton x)] :<=>: insert' xs ms
    lookupRelation i (m :<=>: ms) = (:<->:) <$> S.lookupElem i m <*> lookupRelation i ms

class PolyMapLookup (n :: Nat) (as :: [(*, * -> *)]) where
    -- |Is the key a member at the specified side of the polymap.
    member :: Proxy n -> TypeAt n (MapFst as) -> PolyMap as -> Bool

    -- |Lookup the /index/ of a key, which is its zero-based index in the storage
    -- at the specified side of the polymap. The index is a number from /0/ up
    -- to, but not including, the 'size' of the polymap.
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

-- |Is the key not a member at the specified side of the polymap? See also 'member'.
notMember :: PolyMapLookup n as => Proxy n -> TypeAt n (MapFst as) -> PolyMap as -> Bool
notMember proxy x m = not (member proxy x m)

-- |Lookup the value at a key at the specified side of the polymap.
--
-- The function will return the corresponding value as @('Just' value)@, or
-- 'Nothing' if the key isn't at the specified side of the polymap.
lookup :: (PolyMapClass as, PolyMapLookup n as) => Proxy n -> TypeAt n (MapFst as) -> PolyMap as -> Maybe (Relation (MapFst as))
lookup proxy x m = case lookupIndex proxy x m of
    Nothing -> Nothing
    Just i  -> lookupRelation i m

-- |A polymap with a single relation.
singleton :: (PolyMapClass as, ToRelation a (MapFst as)) => a -> PolyMap as
singleton r = singleton' (toRelation r)

-- |Insert a new relation into the polymap.
insert :: (PolyMapClass as, ToRelation a (MapFst as)) => a -> PolyMap as -> PolyMap as
insert r m = insert' (toRelation r) m
