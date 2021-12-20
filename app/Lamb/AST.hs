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
  ) where

import qualified Data.Text as T

data LambdaExpr = Number Int -- 12
                | Var T.Text
                | Application T.Text (Maybe LambdaExpr) -- f (f x)
                deriving (Eq)

data LambdaAbstraction = Abstraction [T.Text] LambdaExpr
  deriving (Eq, Show)

instance Show LambdaExpr where
  show (Number n)        = show n
  show (Var v)           = show v
  show (Application s e) = "(Application " ++ T.unpack s ++ " " ++ show e ++ ")"
