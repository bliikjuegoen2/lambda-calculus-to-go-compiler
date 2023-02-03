module Main (main) where

import Lib
import System.Environment (getArgs)
import Operation (Operation(runOperation), printErr, ioOutput)
import Data.Function ((&))

main :: IO ()
main = do 
    ["-o",outfile,infile] <- getArgs
    src <- readFile infile
    let output = runOperation compiler src & snd
    printErr output
    code <- ioOutput output "error" 
    writeFile outfile code
    return ()