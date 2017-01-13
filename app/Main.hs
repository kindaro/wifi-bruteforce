#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Control.Arrow

import Shelly
import Data.Text
import Data.Text.IO
import Prelude (IO, (.), ($), fmap)
import qualified Prelude (filter)
default (Text)

main :: IO ()
main = do
    networks <- iwlist
    putStrLn $ unlines networks

-- runner = undefined -- `shelly . silently $ run` can't be expressed point-free...

iwlist :: IO [Text]
iwlist = fmap parse output
    where
        output :: IO Text
        output = shelly . silently $ run "iwlist" ["eno1", "scanning"]

        parse :: Text -> [Text]
        parse = lines
            >>> fmap stripStart
            >>> Prelude.filter (isPrefixOf "ESSID:")
            >>> fmap (drop 6)
            >>> fmap (drop 1 >>> dropEnd 1)

