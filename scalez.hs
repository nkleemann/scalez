-- Ideen:
--      .midi Datei generieren
--      Audio abspielen

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
    | B deriving (Show) -- TODO own instance

data Step
    = Whole | Half deriving (Show)

type SemiTone  = Int
type ScaleName = String

--
--      CONSTANTS
--

notes :: Map.Map SemiTone Note
notes 
    = Map.fromList [
                       (1, C),
                       (2, CSharp),
                       (3, D),
                       (4, DSharp),
                       (5, E),
                       (6, F),
                       (7, FSharp),
                       (8, G),
                       (9, GSharp),
                       (10, A),
                       (11, ASharp),
                       (12, B)
                   ]

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

lookup_note :: SemiTone -> Maybe Note
lookup_note tone = Map.lookup tone notes

lookup_steps :: ScaleName -> Maybe [Step]
lookup_steps name = Map.lookup name scalez

build_from_root :: Note -> ScaleName -> [Note]
build_from_root root name = undefined


