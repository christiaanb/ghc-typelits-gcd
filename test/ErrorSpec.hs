{-# LANGUAGE CPP, DataKinds, TypeOperators #-}

{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.GCD.Solver #-}

module ErrorSpec where

import Control.Exception
import Data.List
import Data.Proxy
import Test.Tasty.HUnit

import GHC.TypeLits.GCD

testFail1 :: Proxy (GCD 6 8) -> Proxy 4
testFail1 = id

testFail1Errors =
  ["Expected type: Proxy (GCD 6 8) -> Proxy 4"
  ,"Actual type: Proxy 4 -> Proxy 4"
  ]

-- | Assert that evaluation of the first argument (to WHNF) will throw
-- an exception whose string representation contains the given
-- substrings.
throws :: a -> [String] -> Assertion
throws v xs = do
  result <- try (evaluate v)
  case result of
    Right _ -> assertFailure "No exception!"
#if MIN_VERSION_base(4,9,0)
    Left (TypeError msg) ->
#else
    Left (ErrorCall msg) ->
#endif
      if all (`isInfixOf` msg) xs
         then return ()
         else assertFailure msg
