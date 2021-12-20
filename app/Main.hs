{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import System.IO

import qualified Data.Text as T
import Lamb.Parser (parse')
import Lamb.Pass (validateAbstraction)

parseInput :: IO ()
parseInput = forever $ do
  putStr ">" >> hFlush stdout
  l <- getLine

  parseResult <- parse' $ T.pack l
  case parseResult of
    Left err -> putStrLn err
    Right val -> case validateAbstraction val of
                    Left ex   -> print ex
                    Right val -> print val


main :: IO ()
main = do
  hSetBuffering stdin LineBuffering

  putStrLn "Welcome to Lamb. Evaluate a lambda calculus expression below:"
  putStrLn "If you wish to use a variable, refer to it prefixed by a ':' like ':x'"

  parseInput
