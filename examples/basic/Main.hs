{-# LANGUAGE DataKinds #-}

import Data.PolyMap (Relation((:<->:)))
import qualified Data.PolyMap as PM

main = sequence $ map print [empty, singleton, two]

empty :: PM.PolyMap '[String, Integer]
empty = PM.empty

singleton :: PM.PolyMap '[String, Integer]
singleton = PM.singleton $ "one" :<->: 1 :<->: PM.UnitRelation

two = PM.insert ("two" :<->: 2 :<->: PM.UnitRelation) singleton
