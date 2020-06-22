{-# LANGUAGE BlockArguments #-}

module Control.Hextra.Parsing where

import Data.Bifunctor
import Control.Applicative
import qualified Experimental.Functors as F

newtype Parser x = Parser { parse :: (String -> ([x], String)) }

instance {- Functor f => -} Functor {- (-} Parser {- f -} where
    fmap f p = Parser \str ->
        first (fmap f) $ parse p str

instance {- Applicative f => -} Applicative {- (-} Parser {- f -} where
    p1 <*> p2 = Parser \str ->
        let (fs, rest1) = parse p1 str
            (xs, rest2) = parse p2 rest1
        in  (fs <*> xs, rest2)
    pure x = Parser \str -> (pure x, str)

instance {- Monad f => -} Monad {- (-} Parser {- f -} where
    join p = Parser \str ->
        let (ps, rest1) = parse p str
            hhhh = unzip $ parse <$> ps <*> pure rest1
        in  undefined
    return = pure