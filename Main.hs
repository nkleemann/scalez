module Main where

import System.Environment
import System.Exit
import Note
import Scale
import Audio

main :: IO ()
main = do
    args <- getArgs
    mapM putStrLn args
    return ()




{-  Example : https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling


    import System.Environment
    import System.Exit
 
    main = getArgs >>= parse >>= putStr . tac
 
    tac  = unlines . reverse . lines
 
    parse ["-h"] = usage   >> exit
    parse ["-v"] = version >> exit
    parse []     = getContents
    parse fs     = concat `fmap` mapM readFile fs
 
    usage   = putStrLn "Usage: tac [-vh] [file ..]"
    version = putStrLn "Haskell tac 0.1"
    exit    = exitWith ExitSuccess
    die     = exitWith (ExitFailure 1)
-}
