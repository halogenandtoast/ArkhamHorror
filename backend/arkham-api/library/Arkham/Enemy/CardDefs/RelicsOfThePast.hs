module Arkham.Enemy.CardDefs.RelicsOfThePast where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

dwellerInThePit :: CardDef
dwellerInThePit =
  unique
    $ doubleSided "90066a"
    $ (enemy "90066b" ("Dweller in the Pit" <:> "Guardian of the Past") RelicsOfThePast 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 6
      , cdCardTraits = setFromList [Creature, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }
