-- Ideen:
--      .midi Datei generieren
--      Audio abspielen
--      Draw mini ASCII Piano Scale Chart

--
-- Für Oktavdarstellung -> [D,E,FSharp,G,A,B,CSharp,D] -> [(D,1),(E,1),(FSharp,1),(G,1),(A,1),(B,1),(CSharp,2),(D,2)]
-- Oder: Funktion, die die Tastatur zeichnet und die Liste durchgeht sieht zwischen zwei Noten von selbst ob sie eine Oktave
-- überspannen. So kann an dieser Stelle reagiert werden. Die Oktavsache kümmert ja eh nur die Zeichen - Funktion.

module SCALEZ where

import qualified Data.Map as Map


--
--      DATA TYPES
--

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
    | B deriving (Show, Enum, Eq) -- TODO own instance

data Step
    = Whole 
    | Half deriving (Show)

type SemiTone  = Int
type ScaleName = String


--
--      LOCAL SCALEZ
--

scalez :: Map.Map ScaleName [Step]
scalez 
    = Map.fromList [
                       ("Major", [Whole, Whole, Half, Whole, Whole, Whole, Half])
                   ]


--
--      FUNCTIONS - MUSIC
--

-- |Perform a whole step.
whole_step :: SemiTone -> SemiTone
whole_step = (+ 2)

-- |Perform a half step.
half_step :: SemiTone -> SemiTone
half_step = (+ 1)

-- |Keep a Tone in one and the same octave.
same_octave :: SemiTone -> SemiTone
same_octave = flip mod 12

-- |Transpose a note by a given step while
-- |staying in the same octave.
transpose :: Note -> Step -> Note
transpose note step
    = case step of
            Half  -> to_note . same_octave . half_step  $ to_tone note
            Whole -> to_note . same_octave . whole_step $ to_tone note

-- |Generate a scale beginning at a root note.
gen_scale :: Note -> [Step] -> [Note] 
gen_scale root steps
    = scanl transpose root steps -- TODO Span octaves!


--
--      FUNCTIONS - UTILS
--

to_tone :: Note -> SemiTone
to_tone = fromEnum

to_note :: SemiTone -> Note
to_note = toEnum

lookup_steps :: ScaleName -> Maybe [Step]
lookup_steps name = Map.lookup name scalez



--      TESTING

major =         [Whole, Whole, Half, Whole, Whole, Whole, Half]
major_from_d =  [D,E,FSharp,G,A,B,CSharp,D]
works =         major_from_d == gen_scale D major 

