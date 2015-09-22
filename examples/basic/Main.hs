{-# LANGUAGE DataKinds #-}

import Data.PolyMap.Nat
import Data.PolyMap (Relation((:<->:)))
import qualified Data.PolyMap as PM

main = do
    sequence $ map print [empty, singleton, twoPM]
    print $ PM.member (Proxy :: Proxy Z) "one" twoPM

empty :: PM.PolyMap '[String, Integer]
empty = PM.empty

singleton :: PM.PolyMap '[String, Integer]
singleton = PM.singleton $ "one" :<->: 1 :<->: PM.UnitRelation

twoPM = PM.insert ("two" :<->: 2 :<->: PM.UnitRelation) singleton
