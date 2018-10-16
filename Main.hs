module Main where

import System.Environment
import System.Exit
import DangerZone
import Note


main :: IO ()
main = do
    args <- getArgs
    parse args
    return ()


-- parse :: [String] -> IO a
parse args =
    case args of
        [r, s]            -> putStrLn "Normal Mode" -- return $ gen_scale r s >> exit -- TODO validate that r is note and s is scale here
        [r, s, "--sing"]  -> putStrLn "Sing a Song"
        ["-v"]            -> version   >> exit
        [_]               -> complain  >> exit
        []                -> usage     >> exit
        _                 -> exit_oh_no


usage       = putStrLn "scalez v 1.0\nUsage: scalez <rootnote> <scale> [--sing]"
complain    = putStrLn "Faulty arguments.\nExample Invocation: scalez F# blues --sing"
version     = putStrLn "Haskell Scalez 1.0"
exit        = exitWith ExitSuccess
exit_oh_no  = exitWith $ ExitFailure 1


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
