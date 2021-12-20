{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Lamb.Parser (parse')

main :: IO ()
main = do
  l <- getLine
  print
    $ parse'
    $ T.pack l
