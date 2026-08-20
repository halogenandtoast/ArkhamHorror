module Arkham.Enemy.CardDefs.ReturnToNightOfTheZealot.ReturnToTheMidnightMasks where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

narogath :: CardDef
narogath =
  unique
    $ doubleSided "50026a"
    $ (enemy "50026b" ("Narôgath" <:> "The Charnel Lord") ReturnToTheMidnightMasks 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Monster, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 2
      }
