{-# LANGUAGE Safe #-}
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
( module Data.PolyMap.Relation
, PolyMap
, Data.PolyMap.null
, size
, member
, notMember
, empty
, singleton
, insert
, relationAt
) where

import Data.PolyMap.Nat
import Data.PolyMap.Relation

listElemAt :: Int -> [a] -> Maybe a
listElemAt i xs
    | i < 0     = Nothing
    | otherwise = f i xs
  where f _ []     = Nothing
        f 0 (x:_)  = Just x
        f i (_:xs) = f (i - 1) xs

type family HasType a (as :: [*]) :: Bool where
    HasType a '[] = 'False
    HasType a (a ': as) = 'True
    HasType a (b ': as) = HasType a as

type family TypeAt (n :: Nat) (as :: [*]) where
    TypeAt 'Z (a ': as) = a
    TypeAt ('S n) (a ': as) = TypeAt n as

data family PolyMap (as :: [*]) :: *
data instance PolyMap '[] = UnitPolyMap
data instance PolyMap (a ': as) = [a] :<=>: PolyMap as

infixr 4 :<=>:

deriving instance Show (PolyMap '[])
deriving instance (Show a, Show (PolyMap as)) => Show (PolyMap (a ': as))

class PolyMapClass (as :: [*]) where
    null :: PolyMap as -> Bool
    size :: PolyMap as -> Int
    empty :: PolyMap as
    singleton' :: Relation as -> PolyMap as
    insert' :: Relation as -> PolyMap as -> PolyMap as
    relationAt :: Int -> PolyMap as -> Maybe (Relation as)

instance PolyMapClass '[] where
    null UnitPolyMap = True
    size UnitPolyMap = 0
    empty = UnitPolyMap
    singleton' UnitRelation = UnitPolyMap
    insert' UnitRelation UnitPolyMap = UnitPolyMap
    relationAt _ UnitPolyMap = Just UnitRelation

instance PolyMapClass as => PolyMapClass (a ': as) where
    null (xs :<=>: _) = Prelude.null xs
    size (xs :<=>: _) = length xs
    empty = [] :<=>: empty
    singleton' (x :<->: xs) = [x] :<=>: singleton' xs
    insert' (x :<->: xs) (m :<=>: ms) = m ++ [x]:<=>: insert' xs ms
    relationAt i (m :<=>: ms) = (:<->:) <$> listElemAt i m <*> relationAt i ms

class PolyMapLookup (n :: Nat) (as :: [*]) where
    member :: Proxy n -> TypeAt n as -> PolyMap as -> Bool

instance PolyMapLookup n '[] where
    member Proxy _ UnitPolyMap = False

instance Eq a => PolyMapLookup 'Z (a ': as) where
    member Proxy x (xs :<=>: _) = elem x xs

instance (PolyMapLookup n as) => PolyMapLookup ('S n) (a ': as) where
    member Proxy x (_ :<=>: ms) = member (Proxy :: Proxy n) x ms

notMember :: PolyMapLookup n as => Proxy n -> TypeAt n as -> PolyMap as -> Bool
notMember proxy x m = not (member proxy x m)

--lookup :: Proxy n -> TypeAt n as -> PolyMap as -> Maybe (TypeAt n as)
--lookup proxy x m = case lookup' Z proxy x m of
--    Nothing -> Nothing
--    Just i  -> 

singleton :: (PolyMapClass as, ToRelation a as) => a -> PolyMap as
singleton r = singleton' (toRelation r)

insert :: (PolyMapClass as, ToRelation a as) => a -> PolyMap as -> PolyMap as
insert r m = insert' (toRelation r) m
