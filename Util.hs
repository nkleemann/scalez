module Util where

import Text.Read (readMaybe)
import Data.Char (toLower, toUpper)
import Note
import Scale
import Audio
import qualified Data.Map as M

--
--  OUTSIDE WORLD
--  HANDLING ZONE
--

-- | Transform a string into it's Note Representation.
strToNote :: String -> Maybe Note
strToNote s
    = case splitAt 1 s of
        (noteStr, "")  -> readMaybe $ map toUpper noteStr :: Maybe Note
        (noteStr, "#") -> readMaybe $ map toUpper noteStr ++ "Sharp" :: Maybe Note
        _              -> Nothing

noteToStr :: Note -> String
noteToStr n
    = case n of
        CSharp -> "C# "
        DSharp -> "D# "
        FSharp -> "F# "
        GSharp -> "G# "
        ASharp -> "A# "
        _      -> show n ++ " "

-- | Search the local scale-DB for a Scale.
strToScale :: String -> ScaleMap -> Maybe Scale
strToScale s = M.lookup $ map toLower s

showScalePattern :: ScalePattern -> IO ()
showScalePattern p
    = print p













genScalePatternH :: Maybe Note -> Maybe Scale -> Maybe ScalePattern
genScalePatternH n s
    = case n of
        Nothing -> Nothing
        Just n' -> case s of
                       Nothing -> Nothing
                       Just s' -> Just $ genScalePattern n' s'


singH :: Maybe Note -> Maybe Scale -> IO ()
singH n s
    = case n of
        Nothing -> return ()
        Just n' -> case s of
                       Nothing -> return ()
                       Just s' -> sing $ toPattern $ freqsFromRoot (toFreq n') s'