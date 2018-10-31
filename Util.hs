module Util where

import Text.Read (readMaybe)
import Data.Char (toLower)
import Note
import Scale
import qualified Data.Map as M

-- | Transform a string into it's Note Representation.
strToNote :: String -> Maybe Note
strToNote s
    = case splitAt 1 s of
        (noteStr, "")  -> readMaybe   noteStr :: Maybe Note
        (noteStr, "#") -> readMaybe $ noteStr ++ "Sharp" :: Maybe Note
        _              -> Nothing

-- | Search the local scale-DB for a Scale.
strToSteps :: String -> ScaleMap -> Maybe [Step]
strToSteps str = M.lookup $ map toLower str

-- | 
genScalePatternH :: Maybe Note -> Maybe Scale -> Maybe ScalePattern
genScalePatternH n s
    = case n of
        Nothing -> Nothing
        Just n' -> case s of
                       Nothing -> Nothing
                       Just s' -> Just $ genScalePattern n' s'

