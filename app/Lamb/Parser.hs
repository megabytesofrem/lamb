{-# LANGUAGE OverloadedStrings #-}

{-|
  Module      : Lamb
  Description : Small lambda calculus interpretor
  License     : MIT
  Maintainer  : megabytesofrem
  Portability : POSIX
-}
module Lamb.Parser
  ( parse
  ) where

-- AST
import           Lamb.AST                       ( LambdaAbstraction(..)
                                                , LambdaExpr(..)
                                                , Variable(..)
                                                )

-- For randomness
import           Control.Monad.Random           ( MonadRandom(getRandomR) )

import           Data.Functor                   ( (<&>) )
import qualified Data.Text                     as T

import           Text.Parsec                    ( (<|>)
                                                , char
                                                , digit
                                                , letter
                                                , many1
                                                , optionMaybe
                                                , parse
                                                , spaces
                                                )
import           Text.Parsec.Char
import           Text.Parsec.Text               ( Parser )
import qualified Text.Parsec.Text              as P

-- | Parse a lambda abstraction
parseAbstraction :: Parser LambdaAbstraction
parseAbstraction = do
  char '/'
  head <- many1 letter
  char '.'
  -- Parse the body
  expr <- T.pack <$> many1 letter
  pure $ Abstraction [Variable $ T.pack head] $ Application expr $ Just
    (Number 5)

-- | Parse a function application
-- f (f x)
parseApplication :: Parser LambdaExpr
parseApplication = do
  i' <- letter <&> (: [])
  spaces

  e <- optionMaybe parseExpr
  pure $ Application (T.pack i') e

-- | Parse a literal number
parseLiteral :: Parser LambdaExpr
parseLiteral = do
  d <- many1 digit
  pure $ Number (read d)

-- | Parse an expression
parseExpr :: Parser LambdaExpr
parseExpr = parseLiteral <|> parseApplication

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
alphaReduce :: (MonadRandom m) => Variable -> m Variable
alphaReduce a = Variable <$> randomLetter

-- -- | Apply an α-conversion to all bound variables in the lambda abstraction
-- -- | and return a new lambda abstraction
-- alphaReduceLC :: (MonadRandom m) => m [Abstraction] -> m [[Variable]]
-- alphaReduceLC as = pure $ vs
--     where vs = [ traverse alphaReduce a | (Abstraction a b) <- as ]
--       -- [ a
--       --   | (Abstraction a b) <- as
--       -- ]