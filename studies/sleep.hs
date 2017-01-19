
import System.Process
import System.Timeout

main = (fmap maybeError $ timeout 3000000 sleep) >>= putStrLn
    where

    maybeError = maybe ("Timed out.") (const "Slept well.")

    sleep = readProcess "sleep" ["2"] ""
