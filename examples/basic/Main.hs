{-# LANGUAGE DataKinds #-}

import Data.PolyMap.Nat
import Data.PolyMap (Relation((:<->:)))
import qualified Data.PolyMap as PM

main = do
    sequence $ map putStrLn [ show empty
                            , show singleton
                            , show twoPM
                            , show (PM.member first "one" twoPM)
                            , show (PM.notMember first "asdf" twoPM)
                            ]

empty :: PM.PolyMap '[String, Integer]
empty = PM.empty

singleton :: PM.PolyMap '[String, Integer]
singleton = PM.singleton $ "one" :<->: 1 :<->: PM.UnitRelation

twoPM = PM.insert ("two" :<->: 2 :<->: PM.UnitRelation) singleton
