module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.HorrorsInTheRock where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

crystalParasite :: CardDef
crystalParasite =
  (enemy "10721" "Crystal Parasite" HorrorsInTheRock 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 6
    , cdCardTraits = setFromList [Monster, Insect, Blight]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }
