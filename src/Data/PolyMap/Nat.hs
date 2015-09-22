{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-|
  Module      : Data.PolyMap.Nat
  Copyright   : (c) 2015 David Farrell
  License     : PublicDomain
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Natural numbers defined at the type level with a proxy defined for the promoted kind.
-}

module Data.PolyMap.Nat where

data Nat = Z | S Nat
data Proxy (a :: Nat) = Proxy
