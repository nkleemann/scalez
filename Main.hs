module Main where

import System.Environment (getArgs)
import Util
import Note
import Scale
import Audio


main :: IO ()
main = do
    args   <- getArgs
    parse args scalez >>= handleArgs 


parse :: [String] -> ScaleMap -> IO (Maybe Note, Maybe Scale, Verbosity)
parse args sz 
    = case args of
        [r, s]            -> return (strToNote r, strToScale s sz, Quiet)
        [r, s, "--sing"]  -> return (strToNote r, strToScale s sz, Loud) 
        ["-v"]            -> version       >> exit
        ["--list"]        -> listScalez sz >> exit
        [_]               -> badArgs       >> exit
        []                -> usage         >> exit
        _                 -> usage         >> exitOhNo

handleArgs :: (Maybe Note, Maybe Scale, Verbosity) -> IO ()
handleArgs args
    = case args of
        (Just n, Just s, Quiet)     -> mapM_ (putStr . noteToStr) (genScalePattern n s) >> putStrLn ""
        (Just n, Just s, Loud)      -> sing $ toPattern $ freqsFromRoot (toFreq n) s

        -- redundany? handle in parse?
        (Nothing, _, _)             -> usage >> return ()
        (_, Nothing, _)             -> usage >> return ()







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
