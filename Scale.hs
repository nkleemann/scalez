module Scale where

import qualified Data.Map as M 

data Step
  = Whole
  | Half
  | AugSec
  deriving (Show, Eq)

type ScaleName = String
type Scale     = (ScaleName, [Step])
type ScaleMap  = M.Map ScaleName [Step]

scalez :: ScaleMap
scalez = M.fromList
    [
        (
            "major",
            [Whole, Whole, Half, Whole, Whole, Whole, Half]
        ),
        (
            "minor", 
            [Whole, Half, Whole, Whole, Half, Whole, Whole]
        ),
        (
            "harmonic-minor", 
            [Whole, Half, Whole, Whole, Half, AugSec, Half]
        ),
        (
            "melodic-minor", 
            [Whole, Half, Whole, Whole, Whole, Whole, Half]
        ),
        (
            "romanian-minor", 
            [Whole, Half, AugSec, Half, Whole, Half, Whole]
        ),
        -- | Named after the Dorian Greeks.
        (
            "dorian-mode", 
            [Whole, Half, Whole, Whole, Whole, Half, Whole]
        ),
        -- | Ancient Greek Scale attributed to Sappho, the 7th-century-B.C. poet and musician.
        (
            "mixolydian-mode", 
            [Whole, Whole, Half, Whole, Whole, Half, Whole]
        ),
        -- | Named after the anciant kingdom of Phrygia in Anatolia.
        (
            "phrygian-mode", 
            [Half, Whole, Whole, Whole, Half, Whole, Whole]
        ),
        -- | Named after the anciant kingdom of Lydia in Anatolia.
        (
            "lydian-mode", 
            [Whole, Whole, Whole, Half, Whole, Whole, Half]
        ),
        -- | Locrian is the word used to describe the inhabitants of the ancient Greek regions of Locris.
        (
            "locrian-mode", 
            [Half, Whole, Whole, Half, Whole, Whole, Whole]
        ),
        -- | Based on the 'Mystic Chord'
        (
            "prometheus",
            [Whole, Whole, Whole, AugSec, Half, Whole]
        ),
        (
            "spanish-gypsy", 
            [Half, AugSec, Half, Whole, Half, Whole, Whole]
        ),
        (
            "blues", 
            [AugSec, Whole, Half, Half, AugSec, Whole]
        ),
        (
            "super-locrian", 
            [Half, Whole, Half, Whole, Whole, Whole, Whole]
        ),
        -- | Still in minor, despite it's name
        (
            "neopolitan-major",
            [Half, Whole, Whole, Whole, Whole, Whole, Half]  
        ),
        (
            "neopolitan-minor",
            [Half, Whole, Whole, Whole, Half, Whole, Whole]
        ),
        -- | originally published in a Milan journal as a musical challenge,
        -- | with an invitation to harmonize it in some way.
        (
            "enigmatic", 
            [Half, AugSec, Whole, Whole, Whole, Half, Half]
        ),
        (
            "pentatonic-neutral",
            [Whole, AugSec, Whole, AugSec]  
        ),
        (
            "pentatonic-major",
            [Whole, AugSec ,Whole, Whole, AugSec]
        ),
        (
            "pentatonic-minor",
            [AugSec, Whole, Whole, AugSec, Whole]  
        ),
        (
            "pentatonic-blues",
            [AugSec, Whole, Half, Half, AugSec]  
        )
    ]
