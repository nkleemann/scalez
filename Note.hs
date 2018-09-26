-- Ideen:
--      Audio abspielen -> runCommand "play -n synth 0.2 sine 440 &> /dev/null" >> return ()
--      midi 

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

type SemiTone  = Int


-- | Perform a half step.
half_step :: SemiTone -> SemiTone
half_step = (+ 1)

-- | Perform a whole step.
whole_step :: SemiTone -> SemiTone
whole_step = (+ 2)

-- | Perform both half and whole step to form an augmented second.
aug_sec :: SemiTone -> SemiTone
aug_sec = (+ 3)

-- | Keep a Tone in one and the same octave.
same_octave :: SemiTone -> SemiTone
same_octave = flip mod 12

-- | Transpose a note by a given step while
-- | staying in the same octave.
transpose :: Note -> Step -> Note
transpose note step
    = case step of
            Half   -> to_note . same_octave . half_step  $ to_tone note
            Whole  -> to_note . same_octave . whole_step $ to_tone note
            AugSec -> to_note . same_octave . aug_sec    $ to_tone note

-- |Generate a scale beginning at a root note.
gen_scale :: Note -> [Step] -> [Note] 
gen_scale root steps
    = scanl transpose root steps


--
--      UTILS
--

-- |Used for transposition
to_tone :: Note -> SemiTone
to_tone = fromEnum

-- |Transform SemiTone back into classical notation as 'Note'
to_note :: SemiTone -> Note
to_note = toEnum





























--      TESTING

major_from_d =  [D,E,FSharp,G,A,B,CSharp,D]
works =         major_from_d == gen_scale D (snd major)