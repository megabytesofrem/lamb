{-# LANGUAGE OverloadedStrings   #-}

{-|
  Module      : Lamb.Parser
  Description : Exceptions and stuff for the interpretor
  License     : MIT
  Maintainer  : megabytesofrem
  Portability : POSIX
-}
module Lamb.Exception
  ( LambException(..)
  ) where

data LambException = FreeVariables

instance Show LambException where
  show FreeVariables = "There was one or more free variables in your abstraction"
