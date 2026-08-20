module Arkham.Enemy.CardDefs.TheDreamEaters.AgentsOfAtlachNacha where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

greyWeaver :: CardDef
greyWeaver =
  (enemy "06084" "Grey Weaver" AgentsOfAtlachNacha 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Spider]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }
