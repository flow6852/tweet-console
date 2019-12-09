{-# LANGUAGE ScopedTypeVariables #-}
module Console where

import TwitterAPI
import Parser
import Data.Text.IO
import Data.Text
import Control.Exception
import System.IO

getCommand :: [String] -> IO ()
getCommand botconf = do
 Prelude.putStr "command $ "
 hFlush stdout
 command <- Data.Text.words <$> Data.Text.IO.getLine
 catch (((parserCommand command) command botconf) >> getCommand botconf) 
       (\(e :: SomeException) -> Prelude.putStrLn ("exit tweet." ++ displayException e) >> return ())
