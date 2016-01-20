{-# LANGUAGE CPP, TupleSections #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module GHC.TypeLits.GCD.Solver
  ( plugin )
where

-- external
import Control.Arrow       ((***))
import Data.List           (partition)
import Data.Maybe          (mapMaybe)
import GHC.TcPluginM.Extra (evByFiat, lookupModule, lookupName)

-- GHC API
import FastString (fsLit)
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm)
import TcPluginM  (TcPluginM, tcLookupTyCon)
import TcRnTypes  (Ct, TcPlugin(..), TcPluginResult (..),
                   ctEvidence, ctEvPred)
import TyCon      (TyCon)
import Type       (EqRel (NomEq), PredTree (EqPred),
                   classifyPredType)
#if __GLASGOW_HASKELL__ >= 711
import TyCoRep    (Type (..), TyLit (..))
#else
import TypeRep    (Type (..), TyLit (..))
#endif

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = \_ -> Just gcdPlugin }

gcdPlugin :: TcPlugin
gcdPlugin =
  TcPlugin { tcPluginInit  = lookupGCDTyCon
           , tcPluginSolve = solveGCD
           , tcPluginStop  = \_ -> return ()
           }

lookupGCDTyCon :: TcPluginM TyCon
lookupGCDTyCon = do
    md      <- lookupModule gcdModule gcdPackage
    gcdTcNm <- lookupName md (mkTcOcc "GCD")
    tcLookupTyCon gcdTcNm
  where
    gcdModule  = mkModuleName "GHC.TypeLits.GCD"
    gcdPackage = fsLit "ghc-typelits-gcd"

solveGCD :: TyCon
         -> [Ct] -- ^ [G]iven constraints
         -> [Ct] -- ^ [D]erived constraints
         -> [Ct] -- ^ [W]anted constraints
         -> TcPluginM TcPluginResult
solveGCD _     _ _ []      = return (TcPluginOk [] [])
solveGCD gcdTc _ _ wanteds = return $! case failed of
    [] -> TcPluginOk (mapMaybe (\c -> (,c) <$> evMagic c) solved) []
    f  -> TcPluginContradiction f
  where
    gcdWanteds :: [(Ct,(Integer,Integer))]
    gcdWanteds = mapMaybe (toGCDEquality gcdTc) wanteds

    solved, failed :: [Ct]
    (solved,failed) = (map fst *** map fst)
                    $ partition (uncurry (==) . snd) gcdWanteds

toGCDEquality :: TyCon -> Ct -> Maybe (Ct,(Integer,Integer))
toGCDEquality gcdTc ct =
  case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2
      -> (ct,) <$> ((,) <$> reduceGCD gcdTc t1
                        <*> reduceGCD gcdTc t2)
    _ -> Nothing

reduceGCD :: TyCon -> Type -> Maybe Integer
reduceGCD gcdTc = go
  where
    go (LitTy (NumTyLit i)) = Just i
    go (TyConApp tc [x,y])
      | tc == gcdTc = gcd <$> go x <*> go y
    go _ = Nothing

evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "ghc-typelits-gcd" t1 t2)
    _                  -> Nothing
