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
  Module      : Data.PolyMap.Relation
  Copyright   : (c) 2015 David Farrell
  License     : PublicDomain
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Relation type family and helper function to create relations from tuples.
-}

module Data.PolyMap.Relation
(
-- * Relation Type
  Relation(..)

-- * Query
, sideAt
, sideOf

-- * Construction
, ToRelation(..)
) where

import Data.PolyMap.Nat

type family TypeAt (n :: Nat) (as :: [*]) where
    TypeAt 'Z (a ': as) = a
    TypeAt ('S n) (a ': as) = TypeAt n as

-- |A relation whose sides are defined by a list of types.
data family Relation (as :: [*])
data instance Relation '[] = UnitRelation
data instance Relation (a ': as) = a :<->: Relation as

infixr 4 :<->:

deriving instance Show (Relation '[])
deriving instance (Show a, Show (Relation as)) => Show (Relation (a ': as))

class RelationSideAt (n :: Nat) (as :: [*]) where
    -- |Retrieve the value at the specified side of the relation.
    sideAt :: Proxy n -> Relation as -> TypeAt n as

instance RelationSideAt 'Z (a ': as) where
    sideAt Proxy (x :<->: _) = x

instance RelationSideAt n as => RelationSideAt ('S n) (a ': as) where
    sideAt Proxy (_ :<->: xs) = sideAt (Proxy :: Proxy n) xs

-- |Infix variant of 'sideAt'.
sideOf :: RelationSideAt n as => Proxy n -> Relation as -> TypeAt n as
sideOf = sideAt
infix 3 `sideOf`

-- |Typeclass for data types that can be used to construct a relation.
class ToRelation a (as :: [*]) where
    -- |Construct a relation from a compatible instance.
    toRelation :: a -> Relation as

instance ToRelation (Relation as) as where toRelation r = r
instance ToRelation ()                                       '[]                                       where
    toRelation ()                                       =                                                                                           UnitRelation
instance ToRelation  a0                                      '[a0]                                     where
    toRelation  x0                                      = x0 :<->:                                                                                  UnitRelation
instance ToRelation (a0, a1)                                 '[a0, a1]                                 where
    toRelation (x0, x1)                                 = x0 :<->: x1 :<->:                                                                         UnitRelation
instance ToRelation (a0, a1, a2)                             '[a0, a1, a2]                             where
    toRelation (x0, x1, x2)                             = x0 :<->: x1 :<->: x2 :<->:                                                                UnitRelation
instance ToRelation (a0, a1, a2, a3)                         '[a0, a1, a2, a3]                         where
    toRelation (x0, x1, x2, x3)                         = x0 :<->: x1 :<->: x2 :<->: x3 :<->:                                                       UnitRelation
instance ToRelation (a0, a1, a2, a3, a4)                     '[a0, a1, a2, a3, a4]                     where
    toRelation (x0, x1, x2, x3, x4)                     = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->:                                              UnitRelation
instance ToRelation (a0, a1, a2, a3, a4, a5)                 '[a0, a1, a2, a3, a4, a5]                 where
    toRelation (x0, x1, x2, x3, x4, x5)                 = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->:                                     UnitRelation
instance ToRelation (a0, a1, a2, a3, a4, a5, a6)             '[a0, a1, a2, a3, a4, a5, a6]             where
    toRelation (x0, x1, x2, x3, x4, x5, x6)             = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->: x6 :<->:                            UnitRelation
instance ToRelation (a0, a1, a2, a3, a4, a5, a6, a7)         '[a0, a1, a2, a3, a4, a5, a6, a7]         where
    toRelation (x0, x1, x2, x3, x4, x5, x6, x7)         = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->: x6 :<->: x7 :<->:                   UnitRelation
instance ToRelation (a0, a1, a2, a3, a4, a5, a6, a7, a8)     '[a0, a1, a2, a3, a4, a5, a6, a7, a8]     where
    toRelation (x0, x1, x2, x3, x4, x5, x6, x7, x8)     = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->: x6 :<->: x7 :<->: x8 :<->:          UnitRelation
instance ToRelation (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) '[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9] where
    toRelation (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->: x6 :<->: x7 :<->: x8 :<->: x9 :<->: UnitRelation
