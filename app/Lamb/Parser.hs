{-# LANGUAGE OverloadedStrings   #-}

{-|
  Module      : Lamb.Parser
  Description : Small lambda calculus interpretor
  License     : MIT
  Maintainer  : megabytesofrem
  Portability : POSIX
-}
module Lamb.Parser
  ( parse'
  ) where

-- AST
import Lamb.AST (LambdaAbstraction (..), LambdaExpr (..))

import Lamb.Exception (LambException)
import Lamb.Pass (validateAbstraction)

-- For randomness
import Control.Monad.Random (MonadRandom (getRandomR))

import Data.Functor ((<&>))
import qualified Data.Text as T

import Text.Parsec (char, digit, letter, many1, optionMaybe, optional, parse,
                    spaces, (<|>))
import Text.Parsec.Char
import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Text as P

-- | Parse a lambda abstraction
abstractionP :: Parser LambdaAbstraction
abstractionP = do
  char '/'
  head <- many1 letter
  char '.'
  -- Parse the body
  Abstraction (T.chunksOf 1 $ T.pack head) <$> exprP

-- | Parse a function application
-- f (f x)
applicationP :: Parser LambdaExpr
applicationP = do
  v <- letter <&> (: [])
  spaces >> optional (char '(')
  e <- optionMaybe exprP
  optional (char ')')
  pure $ Application (T.pack v) e

-- | Parse a bound variable
-- Due to conflicting between function application and variables,
-- variables *must* be prefixed with a :.
variableP :: Parser LambdaExpr
variableP = do
  char ':'
  v <- letter <&> (: [])
  pure $ Var (T.pack v)

-- | Parse a literal number
literalP :: Parser LambdaExpr
literalP = do
  d <- many1 digit
  pure $ Number (read d)

-- | Parse an expression
exprP :: Parser LambdaExpr
exprP = literalP <|> variableP <|> applicationP


parse' :: T.Text -> IO (Either String LambdaAbstraction)
parse' input = do
  let parseResult = parse (spaces >> abstractionP) "lamb" input

  case parseResult of
    Left err  -> pure $ Left ("Error: " ++ show err)
    Right val -> pure $ Right val

-- -- | Apply an α-conversion to all bound variables in the lambda abstraction
-- -- | and return a new lambda abstraction
-- alphaReduceLC :: (MonadRandom m) => m [Abstraction] -> m [[Variable]]
-- alphaReduceLC as = pure $ vs
--     where vs = [ traverse alphaReduce a | (Abstraction a b) <- as ]
--       -- [ a
--       --   | (Abstraction a b) <- as
--       -- ]
