module Main where

import TwitterAPI
import Console
import Parser

main :: IO ()
main = getAPIkeys >>= getCommand
