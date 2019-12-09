{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

import TwitterAPI
import Parser

module Console where

getCommand :: IO ()
getCommand = do
 putStr "command $ "
 command <- getLine
 putChar '\n'
 parserCommand command
 getCommand

