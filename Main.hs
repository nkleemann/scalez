module Main where

import System.Environment (getArgs)
import Util
import Note
import Scale
import Audio

data Flag
  = Sing
  | PrintNotes
  | ListScales
  | ShowUsage
  | BadArgs
  deriving (Eq)


main :: IO ()
main = do
    args <- getArgs
    parse args >>= handleArgs


parse :: [String] -> IO (Maybe Note, Maybe Scale, Flag)
parse args =
    case args of
        [r, s]            -> return (strToNote r, strToScale s, PrintNotes)
        [r, s, "--sing"]  -> return (strToNote r, strToScale s, Sing) 
        ["--list"]        -> return (Nothing, Nothing, ListScales)
        []                -> return (Nothing, Nothing, ShowUsage)
        _                 -> return (Nothing, Nothing, BadArgs)

handleArgs :: (Maybe Note, Maybe Scale, Flag) -> IO ()
handleArgs args = 
    case args of
        (Just n, Just s, PrintNotes) -> mapM_ (putStr . noteToStr) (genScalePattern n s) >> putStrLn ""
        (Just n, Just s, Sing)       -> sing $ toPattern $ freqsFromRoot (toFreq n) s
        (_, _, ListScales)           -> listScalez >> exit
        (_, _, ShowUsage)            -> usage >> exit
        (_, _, BadArgs)              -> badArgs >> exit
        _                            -> badArgs >> exitOhNo








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
