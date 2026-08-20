module Arkham.Enemy.CardDefs.GuardiansOfTheAbyss.TheEternalSlumber where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

creatureFromTheAbyss :: CardDef
creatureFromTheAbyss =
  (enemy "83015" "Creature from the Abyss" TheEternalSlumber 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fightX
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Dreamlands]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

humbleSupplicant :: CardDef
humbleSupplicant =
  (enemy "83014" "Humble Supplicant" TheEternalSlumber 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

neith :: CardDef
neith =
  unique
    $ (enemy "83013" ("Neith" <:> "Harbinger of the Abyss") TheEternalSlumber 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Humanoid, Brotherhood, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }
