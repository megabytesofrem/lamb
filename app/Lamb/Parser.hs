{-# LANGUAGE OverloadedStrings #-}

{-|
  Module      : Lamb
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

-- For randomness
import Control.Monad.Random (MonadRandom (getRandomR))

import Data.Functor ((<&>))
import qualified Data.Text as T

import Text.Parsec (char, digit, letter, many1, optionMaybe, parse, spaces,
                    (<|>))
import Text.Parsec.Char
import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Text as P

-- | Parse a lambda abstraction
parseAbstraction :: Parser LambdaAbstraction
parseAbstraction = do
  char '/'
  head <- many1 letter
  char '.'
  -- Parse the body
  Abstraction (T.chunksOf 1 $ T.pack head) <$> parseExpr

-- | Parse a function application
-- f (f x)
parseApplication :: Parser LambdaExpr
parseApplication = do
  v <- letter <&> (: [])
  spaces >> char '('
  e <- optionMaybe parseExpr
  char ')'
  pure $ Application (T.pack v) e

-- | Parse a bound variable
-- Due to conflicting between function application and variables,
-- variables *must* be prefixed with a :.
parseVariable :: Parser LambdaExpr
parseVariable = do
  char ':'
  v <- letter <&> (: [])
  pure $ Var (T.pack v)

-- | Parse a literal number
parseLiteral :: Parser LambdaExpr
parseLiteral = do
  d <- many1 digit
  pure $ Number (read d)

-- | Parse an expression
parseExpr :: Parser LambdaExpr
parseExpr = parseLiteral <|> parseVariable <|> parseApplication 

parse' :: T.Text -> Either String LambdaAbstraction
parse' input = case parse (spaces >> parseAbstraction) "lamb" input of
  Left  err -> Left $ "Error: " ++ show err
  Right val -> pure val


-- | Use MonadRandom to generate a random letter for use later when we
-- perform α-conversion
randomLetter :: (MonadRandom m) => m T.Text
randomLetter = do
  r <- getRandomR (0, 25)
  pure $ T.singleton $ letters !! r
  where letters = "abcdefghijklmnopqrstuvwxyz"

-- | Perform α-conversion/reduction to rename bound variables and avoid
-- naming conflicts
alphaReduce :: (MonadRandom m) => T.Text -> m T.Text
alphaReduce a = randomLetter

-- -- | Apply an α-conversion to all bound variables in the lambda abstraction
-- -- | and return a new lambda abstraction
-- alphaReduceLC :: (MonadRandom m) => m [Abstraction] -> m [[Variable]]
-- alphaReduceLC as = pure $ vs
--     where vs = [ traverse alphaReduce a | (Abstraction a b) <- as ]
--       -- [ a
--       --   | (Abstraction a b) <- as
--       -- ]
