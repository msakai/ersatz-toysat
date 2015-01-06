{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Masahiro Sakai 2014
-- License   :  BSD3
-- Maintainer:  Masahiro Sakai <masahiro.sakai@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
--------------------------------------------------------------------
module Ersatz.Solver.Toysat where

import Control.Monad
import Control.Monad.IO.Class
import Data.Array.IArray
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Ersatz
import qualified ToySolver.SAT as ToySAT

toysat :: MonadIO m => Solver SAT m
toysat problem = liftIO $ do
  solver <- ToySAT.newSolver
  let nv = dimacsNumVariables problem
  ToySAT.newVars_ solver nv
  forM_ (Set.toList (dimacsClauses problem)) $ \clause -> do
    ToySAT.addClause solver (IntSet.toList clause)
  ret <- ToySAT.solve solver
  if ret then do
    model <- ToySAT.getModel solver
    return (Satisfied, IntMap.fromList (assocs model))
  else
    return (Unsatisfied, IntMap.empty)
