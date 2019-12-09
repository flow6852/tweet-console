{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Parser ( parserCommand ) where

import TwitterAPI
import Data.Text
import Data.Text.IO

parserCommand :: [Text] -> ([Text] -> [String] -> IO())
parserCommand command = case Prelude.head command of "tweet" -> tweetCommand (Prelude.tail command)
                                                     "dm"    -> dmCommand (Prelude.tail command)
                                                     "help"  -> helpCommand
                                                     _       -> errorCommand

tweetCommand :: [Text] -> ([Text] -> [String] -> IO())
tweetCommand command = case Prelude.head command of "get"  -> tweetGetCommand
                                                    "post" -> tweetPostCommand
                                                    "rm"   -> tweetRmCommand
                                                    _      -> errorCommand

dmCommand :: [Text] -> ([Text] -> [String] -> IO())
dmCommand command = case Prelude.head command of "get"  -> dmGetCommand 
                                                 "post" -> dmPostCommand
                                                 _      -> errorCommand

tweetGetCommand command botconf = do
 tl <- (\x -> case x of Left e  -> error e
                        Right l -> l      ) <$> getTL botconf
 printTimeLine tl
  where
   printTimeLine :: [GetTL] -> IO()
   printTimeLine [] = return ()
   printTimeLine (t:tl) = do
    Prelude.putStrLn "================================="
    Data.Text.IO.putStrLn $ gtl_text t
    Prelude.putStr "id :: "
    Data.Text.IO.putStrLn $ gtl_id_str t
    Prelude.putStr "screen_name :: "
    Data.Text.IO.putStrLn $ (gur_screen_name.gtl_user) t
    printTimeLine tl
 
tweetPostCommand command botconf = tweet (Data.Text.unwords (Prelude.drop 2 command)) "" botconf >> return ()
tweetRmCommand command botconf = rmTweet (command !! 2) botconf >> return ()

dmGetCommand :: [Text] -> [String] -> IO()
dmGetCommand command botconf = do
 dm <- (\x -> case x of Left e  -> error e
                        Right l -> gdm_events l) <$> getDM botconf
 printDM dm
  where
   printDM :: [GetEvents] -> IO()
   printDM [] = return ()
   printDM (d:dm) = do
    Prelude.putStrLn "================================="
    Data.Text.IO.putStrLn $ (gmd_text.gmc_message_data.gev_message_create) d
    Prelude.putStr "id :: "
    Data.Text.IO.putStrLn $ (gmc_sender_id.gev_message_create) d
    printDM dm
 

dmPostCommand :: [Text] -> [String] -> IO()
dmPostCommand command botconf = do
 let screen_name = command !! 2
     text = command !! 3
 id <- (\x -> case x of Left e  -> error e
                        Right l -> (gur_id_str.Prelude.head) l) <$> getUser text botconf
 postDM text id botconf

errorCommand :: [Text] -> [String] -> IO()
errorCommand command botconf = do
 Prelude.putStr "error :: "
 Data.Text.IO.putStrLn $ Data.Text.unwords command

helpCommand :: [Text] -> [String] -> IO()
helpCommand command botconf = do
 Prelude.putStrLn "usage"
 Prelude.putStrLn "tweet get ... get your timeline"
 Prelude.putStrLn "tweet post text ... post text"
 Prelude.putStrLn "tweet rm id ... remove tweet id"
 Prelude.putStrLn "dm get ... get all direct message"
 Prelude.putStrLn "dm post id text ... sending directmessage for id"
