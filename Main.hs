module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Util
import Note
import Scale
import Audio


main :: IO ()
main = do
    args   <- getArgs
    result <- parse args scalez
    print result
    return ()


parse :: [String] -> ScaleMap -> IO (Maybe Note, Maybe Scale, Verbosity)
parse args sz =
    case args of
        [r, s]            -> return (strToNote r, strToSteps s sz, Quiet) 
        [r, s, "--sing"]  -> return (strToNote r, strToSteps s sz, Loud) 
        ["-v"]            -> version   >> exit
        [_]               -> complain  >> usage >> exit
        []                -> usage     >> exit
        _                 -> usage     >> exitOhNo




usage, complain, version :: IO ()
usage       = putStrLn "scalez v 1.0\nUsage: scalez <rootnote> <scale> [--sing]"
complain    = putStrLn "Errror: Faulty arguments.\n"
version     = putStrLn "Haskell Scalez 1.0"

exit, exitOhNo :: IO a
exit      = exitWith ExitSuccess
exitOhNo  = exitWith $ ExitFailure 1


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

-- sing $ to_pattern (freqs_from_root (to_freq C) (snd major))
