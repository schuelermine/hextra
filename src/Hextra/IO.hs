{-# LANGUAGE ExplicitForAll #-}

module Hextra.IO where

processInput :: forall a. (String -> IO a) -> IO a
processInput = (getLine >>=)

processInputRead :: forall a b. Read b => (b -> IO a) -> IO a
processInputRead f = (getLine >>= f . read)

processPrompt :: forall a. Maybe String -> (String -> IO a) -> IO a
processPrompt a m = case a of
    Just p -> putStr p >> processInput m
    Nothing -> processInput m

processPromptRead :: forall a b. Read b => Maybe String -> (b -> IO a) -> IO a
processPromptRead a m = case a of
    Just p -> putStr p >> processInputRead m
    Nothing -> processInputRead m

prompt :: String -> IO String
prompt p = do
    putStr p
    getLine