-- Ideen:
--      .midi Datei generieren
--      Audio abspielen
--      Draw mini ASCII Piano Scale Chart

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
    | B deriving (Show, Enum) -- TODO own instance

data Step
    = Whole 
    | Half deriving (Show, Enum)

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

whole_step :: SemiTone -> SemiTone
whole_step = (+ 2)

half_step  :: SemiTone -> SemiTone
half_step  = (+ 1)


--
--      FUNCTIONS - UTILS
--

to_tone :: Note -> SemiTone
to_tone = fromEnum

to_note :: SemiTone -> Note
to_note = toEnum

lookup_steps :: ScaleName -> Maybe [Step]
lookup_steps name = Map.lookup name scalez

build_from_root :: Note -> ScaleName -> [Note]
build_from_root root name = undefined


