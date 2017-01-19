import System.Process
import GHC.IO.Handle
main = do
    (in_, out, err, pid) <- createProcess (proc "date" []) { std_out = CreatePipe }
    resp <- maybe (error "No stdout handle!") hGetLine out
    putStrLn resp
    
