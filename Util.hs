module Util where

import Text.Read
import Note
import Scale

-- | Transform a string into a it's Note Representation.
strToNote :: String -> Maybe Note
strToNote s
    = case splitAt 1 s of
        (noteStr, "")  -> readMaybe   noteStr :: Maybe Note
        (noteStr, "#") -> readMaybe $ noteStr ++ "Sharp" :: Maybe Note
        _               -> Nothing

strToScale :: String -> Maybe Scale
strToScale s
    = undefined


scaleHelper :: Maybe Note -> Maybe Scale -> Maybe [Note]
scaleHelper n s
    = case n of
        Nothing -> Nothing
        Just n' -> case s of
                       Nothing -> Nothing
                       Just s' -> Just $ genScale n' (snd s')

