module Util where

import Note
import Scale

scales =
  [ ("2122221", "Minor")
  , ("2122131", "Harmonic Minor")
  , ("2122221", "Melodic Minor")
  , ("23223", "Pentatonic Major")
  , ("32232", "Pentatonic Minor")
  , ("32113", "Pentatonic Blues")
  , ("2323", "Pentatonic Neutral")
  , ("2212221", "Ionian")
  , ("32122122", "Aeolian")
  , ("2122212", "Dorian")
  , ("2212212", "Mixolydian")
  , ("1222122", "Phrygian")
  , ("2221221", "Lydian")
  , ("1221222", "Locrian")
  , ("1212121", "Dim half")
  , ("2121212", "Dim whole")
  , ("22222", "Whole")
  , ("31313", "Augmented")
  , ("111111111111", "Chromatic")
  , ("2131212", "Roumanian Minor")
  , ("1312122", "Spanish Gypsy")
  , ("321132", "Blues")
  , ("22323", "Diatonic")
  , ("1312131", "Double Harmonic")
  , ("12111222", "Eight Tone Spanish")
  , ("1322211", "Enigmatic")
  , ("222211", "Leading Whole Tone")
  , ("2222121", "Lydian Augmented")
  , ("1222221", "Neoploitan Major")
  , ("1222122", "Neopolitan Minor")
  , ("222312", "Prometheus")
  , ("132312", "Prometheus Neopolitan")
  , ("131313", "Six Tone Symmetrical")
  , ("1212222", "Super Locrian")
  , ("2221122", "Lydian Minor")
  , ("2131122", "Lydian Diminished")
  , ("211211121", "Nine Tone Scale")
  , ("21212121", "Auxiliary Diminished")
  , ("222222", "Auxiliary Augmented")
  , ("12121212", "Auxiliary Diminished Blues")
  , ("2211222", "Major Locrian")
  , ("2221212", "Overtone")
  , ("1212222", "Diminished Whole Tone")
  , ("2122122", "Pure Minor")
  ]

parse :: [(String, String)] -> [(ScaleName, [Step])]
parse list = map (\(step_str, name) -> (name, map to_step step_str)) list

to_step :: Char -> Step
to_step c =
  case c of
    '1' -> Half
    '2' -> Whole
    '3' -> AugSec
