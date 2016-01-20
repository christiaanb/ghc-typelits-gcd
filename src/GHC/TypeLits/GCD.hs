{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module GHC.TypeLits.GCD
  ( GCD )
where

import GHC.TypeLits (Nat)

type family GCD (x :: Nat) (y :: Nat) :: Nat where
  GCD 0 x = x
