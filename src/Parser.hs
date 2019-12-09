{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Parser ( parserCommand ) where

parserCommand :: [String] -> [String] -> IO ()
parserCommand command botconf = case head command of "tweet" -> tweetCommand (last command) botconf
                                                     "dm"    -> dmCommand (last command) botconf
                                                     "exit"  -> exitCommand

tweetCommand :: [String] -> [String] -> IO ()
tweetCommand command botconf = case head command of "get"  -> tweetGetCommand botconf
                                                    "post" -> tweetPostCommand (last command) botconf
                                                    "rm"   -> tweetRmCommand (last command) botconf

dmCommand :: [String] -> [String] -> IO()
dmCommand command botconf = case head command of "get"  -> dmGetCommand (last command) botconf
                                                 "post" -> dmPostCommand (last command) botconf

tweetGetCommand botconf = do
 tl <- (\x -> case x of Right e -> error e
                        Left l  -> l      ) <$> getTL botconf
 printTimeLine tl
  where
   printTimeLine :: [GetTL] -> IO()
   printTimeLine [] = return ()
   printTimeLine (t:tl) = do
    putStrLn "================================="
    putStrLn $ gtl_text t
    putStr "id :: "
    putStrLn $ gtl_id_str t
    putStr "screen_name :: "
    putStrLn $ (gur_screen_name.gtl_user) t
    printTimeLine tl
 
tweetPostCommand command botconf = tweet command "" botconf
tweetRmCommand command botconf = rmTweet command botconf

dmGetCommand = getDM botconf
dmGetCommand :: String -> [String] -> IO()
dmGetCommand screen_name botconf = do
 id <- (\x -> case x of Right e -> error e
                        Left l  -> l      ) <$> getUser screen_name botconf
 printDM $ gdm_events dm 
  where
   printDM :: [GetEvents] -> IO()
   printDM [] = return ()
   printDM (d:dm) = do
    putStrLn "================================="
    putStrLn $ (gmd_text.gmc_message_data.gev_message_create) d
    putStr "id :: "
    putStrLn $ () d
    printDM dm
 

