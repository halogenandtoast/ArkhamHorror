module Arkham.Enemy.CardDefs.NightOfTheZealot.TheMidnightMasks where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

theMaskedHunter :: CardDef
theMaskedHunter =
  unique
    $ doubleSided "01121a"
    $ ( enemy
          "01121b"
          ("The Masked Hunter" <:> "Silently Stalking")
          TheMidnightMasks
          1
      )
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 2
      }
