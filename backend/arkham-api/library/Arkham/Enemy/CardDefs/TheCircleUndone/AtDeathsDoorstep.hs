module Arkham.Enemy.CardDefs.TheCircleUndone.AtDeathsDoorstep where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

josefMeiger :: CardDef
josefMeiger =
  unique
    $ doubleSided "05085b"
    $ (enemy "05085" ("Josef Meiger" <:> "Lodge Host") AtDeathsDoorstep 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight, Elite]
      , cdKeywords = singleton Keyword.Retaliate
      , cdVictoryPoints = Just 2
      }
