{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.GCD.Solver #-}

import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit

import GHC.TypeLits.GCD
import ErrorSpec

test1 :: Proxy (GCD 6 8) -> Proxy 2
test1 = id

test2 :: Proxy (GCD 0 x) -> Proxy x
test2 = id

tests :: TestTree
tests = testGroup "ghc-typelits-gcd"
  [ testGroup "Basic functionality"
    [ testCase "GCD 6 8 ~ 2" $
      show (test1 Proxy) @?= "Proxy"
    , testCase "GCD 0 x ~ x" $
      show (test2 Proxy) @?= "Proxy"
    ]
  , testGroup "errors"
    [ testCase "GCD 6 8 ~ 4" $ testFail1 `throws` testFail1Errors
    ]
  ]

main :: IO ()
main = defaultMain tests
