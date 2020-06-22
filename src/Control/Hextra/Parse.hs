{-# LANGUAGE BlockArguments #-}

module Control.Hextra.Parsing where

import Data.Bifunctor
import Control.Applicative
import qualified Experimental.Functors as F

newtype Parser x = Parser { parse :: (String -> [(x, String)]) }

instance Functor Parser where
    fmap f p = Parser \str ->
        first f <$> parse p str
--
--instance Applicative Parser where
--    p1 <*> p2 = Parser \str ->
--        let 

-- instance {- Applicative f => -} Applicative {- (-} Parser {- f -} where
--     p1 <*> p2 = Parser \str ->
--         let (fs, rest1) = parse p1 str
--             (xs, rest2) = parse p2 rest1
--         in  (fs <*> xs, rest2)
--     pure x = Parser \str -> (pure x, str)

-- instance {- Monad f => -} F.Monad {- (-} Parser {- f -} where
--     join p = Parser \str ->
--         let (ps, rest1) = parse p str
--             (xs, rest2) = first unzip $ parse <$> ps <*> pure rest1
--         in  (xs, rest2)
--     --p1 >>= k = Parser \str ->
--     --    let (xs, rest1) 
--     return = pure