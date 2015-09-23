{-# LANGUAGE Safe #-}
{-# LANGUAGE DataKinds #-}

import Data.PolyMap.Nat
import qualified Data.PolyMap as PM

main = do
    sequence $ map putStrLn [ show empty
                            , show singleton
                            , show twoPM
                            , show (PM.member first "one" twoPM)
                            , show (PM.notMember first "asdf" twoPM)
                            , show (PM.lookupIndex first "two" twoPM)
                            , show (PM.relationAt 2 twoPM)
                            , show (PM.lookup first "two" twoPM)
                            ]

empty :: PM.PolyMap '[String, Integer]
empty = PM.empty

singleton :: PM.PolyMap '[String, Integer]
singleton = PM.singleton ("one", 1 :: Integer)

twoPM = PM.insert ("two", 2 :: Integer) singleton
