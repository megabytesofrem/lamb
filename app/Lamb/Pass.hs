{-# LANGUAGE OverloadedStrings #-}

{-|
  Module      : Lamb.Pass
  Description : Perform passes such as resolving free variables
  License     : MIT
  Maintainer  : megabytesofrem
  Portability : POSIX
-}

module Lamb.Pass
  ( checkFreeVariables )
where

import Lamb.AST (LambdaAbstraction (Abstraction), LambdaExpr (..))

import Data.Either (rights)
import Data.Maybe

import qualified Data.Text as T
import Debug.Trace (trace)

-- ! Forcefully unwraps a Right value from a Either
-- ! Only used for debugging
unwrapRight :: Either a b -> b
unwrapRight a = (head . rights) [a]

checkExpr :: T.Text -> LambdaExpr -> (T.Text, Bool)
checkExpr v (Var v')                  = (v, v == v')
checkExpr v (Number _)                = (v, False)
checkExpr v (Application f e')        = case e' of
                                          Just e' -> (v, f == v || snd (checkExpr v e'))
                                          Nothing -> (v, False)

-- | Check the entire abstraction to make sure there are no free variables
-- |
-- Return a tuple with the expression, and a list of tuples corresponding to the variable
-- and a boolean representing whether it was bound.
checkFreeVariables :: LambdaAbstraction -> (LambdaExpr, [(T.Text, Bool)])
checkFreeVariables (Abstraction vs e) = (e, checked)
  where
    checked  = map (`checkExpr` e) vs
