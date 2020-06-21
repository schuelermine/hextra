{-# LANGUAGE NoMonomorphismRestriction, ExplicitForAll #-}

module Extra where

import Extra.Function

(%) :: forall i. Integral i => i -> i -> i
(%) = mod

iff a b c = if a then b else c

(#) = elem
($?) = uncurry3' iff
(!) = seq
(§) = uncurry
(<=>) = compare
(?+) = max
(?-) = min

processInput :: forall a. (String -> IO a) -> IO a
processInput = (getLine >>=)

processInputRead :: forall a b. Read b => (b -> IO a) -> IO a
processInputRead f = (getLine >>= f . read)

processPrompt :: forall a. Maybe String -> (String -> IO a) -> IO a
processprompt a m = case a of
    Just p -> putStr p >> processInput m
    Nothing -> processInput m

processPromptRead :: forall a b. Read b => Maybe String -> (b -> IO a) -> IO a
processinput_read a m = case a of
    Just p -> putStr p >> processInput_Read m
    Nothing -> processInput_Read m

wrapunwrap :: (a -> b, b -> a) -> (b -> b) -> a -> a
wrapunwrap (wrap, unwrap) f = unwrap . f . wrap

symmetrical :: (a -> b) -> (a -> b -> Bool) -> a -> Bool
symmetrical f g a = g a (f a)