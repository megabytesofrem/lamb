{-# LANGUAGE OverloadedStrings #-}

{-|
  Module      : Lamb.Pass
  Description : Perform passes such as resolving free variables
  License     : MIT
  Maintainer  : megabytesofrem
  Portability : POSIX
-}

module Lamb.Pass
  ( -- Free variables
    findVariables,
    validateAbstraction,

    -- Alpha reduction
    alphaReduce,

    -- Debugging
    unwrapRight )
where

import Lamb.AST (LambdaAbstraction (Abstraction), LambdaExpr (..))
import Lamb.Exception (LambException (..))

import Data.Either (rights)
import Data.Maybe

import Control.Monad.Random (MonadRandom (getRandomR))
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
                                          -- if f is v, or calling checkExpr recursively on the sub expression
                                          -- returns True then we have a successfully bound value
                                          Just e' -> (v, f == v || snd (checkExpr v e'))
                                          Nothing -> (v, False)

-- | Return a tuple with the variable name and whether or not it was found
findVariables :: LambdaAbstraction -> [(T.Text, Bool)]
findVariables (Abstraction vs e) = map (`checkExpr` e) vs

-- | Validate the abstraction and throw a `Control.Exception` if any
-- free variables are found.
validateAbstraction :: LambdaAbstraction -> Either LambException LambdaAbstraction
validateAbstraction abs =
  if (not . null) free
    then Left FreeVariables
    else Right abs
  where
    -- list of all the variables, True means we have a bound variable
    all = map snd $ findVariables abs
    free = [x | x <- all, not x]

-- | Use MonadRandom to generate a random letter for use later when we
-- perform α-conversion
randomLetter :: (MonadRandom m) => m T.Text
randomLetter = do
  r <- getRandomR (0, 25)

  pure . T.singleton $ letters !! r
  where letters = "abcdefghijklmnopqrstuvwxyz"

-- | Perform α-conversion/reduction to rename bound variables and avoid
-- naming conflicts
alphaReduce :: (MonadRandom m) => T.Text -> m T.Text
alphaReduce a = randomLetter
