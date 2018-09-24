module SCALEZ where

import qualified Data.Map as Map

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
    = Whole | Half

data Scale 
    = Scale
      {
          steps :: [Step]
      }

type SemiTone = Int

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

scalez :: Map.Map String Scale
scalez 
    = Map.fromList [
                       ("Major", Scale {steps=[Whole, Whole, Half, Whole, Whole, Whole, Half]})
                   ]

to_note :: SemiTone -> Maybe Note
to_note tone = Map.lookup tone notes

whole_step :: SemiTone -> SemiTone
whole_step = (+ 2)

half_step  :: SemiTone -> SemiTone
half_step  = (+ 1)

