module Note where

import Scale

data Note
    = C 
    | CSharp 
    | D 
    | DSharp 
    | E 
    | F 
    | FSharp
    | G
    | GSharp
    | A
    | ASharp
    | B deriving (Show, Enum, Eq)

type SemiTone = Int


-- | Perform a half step.
half_step :: SemiTone -> SemiTone
half_step = (+ 1)

-- | Perform a whole step.
whole_step :: SemiTone -> SemiTone
whole_step = (+ 2)

-- | Perform both half and whole step to form an 'augmented second'.
aug_sec :: SemiTone -> SemiTone
aug_sec = (+ 3)

-- | Keep a tone in the same octave.
same_octave :: SemiTone -> SemiTone
same_octave = flip mod 12

-- | Transpose a note by a given step.
transpose :: Note -> Step -> Note
transpose note step
    = case step of
            Half   -> to_note . same_octave . half_step  $ to_tone note
            Whole  -> to_note . same_octave . whole_step $ to_tone note
            AugSec -> to_note . same_octave . aug_sec    $ to_tone note

-- | Generate a concrete scale beginning at root note.
gen_scale :: Note -> [Step] -> [Note] 
gen_scale = scanl transpose


--
--      UTILS
--

-- | Used for transposition
to_tone :: Note -> SemiTone
to_tone = fromEnum

-- | Transform SemiTone back into classical notation as 'Note'
to_note :: SemiTone -> Note
to_note = toEnum








