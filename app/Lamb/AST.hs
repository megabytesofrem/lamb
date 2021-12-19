{-# LANGUAGE OverloadedStrings #-}

{-|
  Module      : Lamb
  Description : Small lambda calculus interpretor
  License     : MIT
  Maintainer  : megabytesofrem
  Portability : POSIX
-}
module Lamb.AST
  ( LambdaExpr(..)
  , LambdaAbstraction(..)
  , Variable(..)
  ) where

import qualified Data.Text                     as T

-- | A variable represented in the Lambda Calculus
newtype Variable = Variable T.Text deriving (Eq, Show)

data LambdaExpr
  = Number Int -- 12
  | Application T.Text (Maybe LambdaExpr) -- f (f x)
  deriving (Eq)

data LambdaAbstraction = Abstraction [Variable] LambdaExpr
  deriving (Eq, Show)

instance Show LambdaExpr where
  show (Number n) = show n
  show (Application s e) =
    "(Application " ++ T.unpack s ++ " " ++ show e ++ ")"
