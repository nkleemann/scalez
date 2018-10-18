module Scale where


data Step
  = Whole
  | Half
  | AugSec
  deriving (Show, Eq)

type ScaleName = String
type Scale = (ScaleName, [Step])

-- TODO Scale map to look up a scale


major :: Scale
major
    = (
          "major",
          [Whole, Whole, Half, Whole, Whole, Whole, Half]
      )

minor :: Scale
minor 
    = (
          "minor", 
          [Whole, Half, Whole, Whole, Half, Whole, Whole]
      )

harmonic_minor :: Scale
harmonic_minor 
    = (
          "harmonic-minor", 
          [Whole, Half, Whole, Whole, Half, AugSec, Half]
      )

melodic_minor :: Scale
melodic_minor 
    = (
          "melodic-minor", 
          [Whole, Half, Whole, Whole, Whole, Whole, Half]
      )

romanian_minor :: Scale
romanian_minor 
    = (
          "romanian-minor", 
          [Whole, Half, AugSec, Half, Whole, Half, Whole]
      )

-- | Named after the Dorian Greeks.
dorian_mode :: Scale
dorian_mode 
    = (
          "dorian-mode", 
          [Whole, Half, Whole, Whole, Whole, Half, Whole]
      )

-- | Ancient Greek Scale attributed to Sappho, the 7th-century-B.C. poet and musician.
mixolydian_mode :: Scale
mixolydian_mode 
    = (
          "mixolydian-mode", 
          [Whole, Whole, Half, Whole, Whole, Half, Whole]
      )

-- | Named after the anciant kingdom of Phrygia in Anatolia.
phrygian_mode :: Scale
phrygian_mode 
    = (
          "phrygian-mode", 
          [Half, Whole, Whole, Whole, Half, Whole, Whole]
      )

-- | Named after the anciant kingdom of Lydia in Anatolia.
lydian_mode :: Scale
lydian_mode 
    = (
          "lydian-mode", 
          [Whole, Whole, Whole, Half, Whole, Whole, Half]
      )

-- | Locrian is the word used to describe the inhabitants of the ancient Greek regions of Locris.
locrian_mode :: Scale
locrian_mode 
    = (
          "locrian-mode", 
          [Half, Whole, Whole, Half, Whole, Whole, Whole]
      )

-- | Based on the 'Mystic Chord'
prometheus :: Scale
prometheus
    = (
          "prometheus",
          [Whole, Whole, Whole, AugSec, Half, Whole]
      )

spanish_gypsy :: Scale
spanish_gypsy
    = (
          "spanish-gypsy", 
          [Half, AugSec, Half, Whole, Half, Whole, Whole]
      )

blues :: Scale
blues
    = (
          "blues", 
          [AugSec, Whole, Half, Half, AugSec, Whole]
      )

super_locrian :: Scale
super_locrian
    = (
          "super-locrian", 
          [Half, Whole, Half, Whole, Whole, Whole, Whole]
      )

-- | Still in minor, despite it's name
neopolitan_major :: Scale
neopolitan_major
    = (
          "neopolitan-major",
          [Half, Whole, Whole, Whole, Whole, Whole, Half]  
      )

neopolitan_minor :: Scale
neopolitan_minor
    = (
          "neopolitan-minor",
          [Half, Whole, Whole, Whole, Half, Whole, Whole]
      )


-- | originally published in a Milan journal as a musical challenge,
-- | with an invitation to harmonize it in some way.
enigmatic :: Scale
enigmatic
    = (
          "enigmatic", 
          [Half, AugSec, Whole, Whole, Whole, Half, Half]
      )

pentatonic_neutral :: Scale
pentatonic_neutral
    = (
          "pentatonic-neutral",
          [Whole, AugSec, Whole, AugSec]  
      )

pentatonic_major :: Scale
pentatonic_major
    = (
          "pentatonic-major",
          [Whole, AugSec ,Whole, Whole, AugSec]
      )


pentatonic_minor :: Scale
pentatonic_minor
    = (
          "pentatonic-minor",
          [AugSec, Whole, Whole, AugSec, Whole]  
      )

pentatonic_blues :: Scale
pentatonic_blues
    = (
          "pentatonic-blues",
          [AugSec, Whole, Half, Half, AugSec]  
      )
