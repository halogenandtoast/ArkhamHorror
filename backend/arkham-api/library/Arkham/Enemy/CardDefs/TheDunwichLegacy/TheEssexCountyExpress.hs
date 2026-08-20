module Arkham.Enemy.CardDefs.TheDunwichLegacy.TheEssexCountyExpress where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

emergentMonstrosity :: CardDef
emergentMonstrosity =
  (enemy "02183" "Emergent Monstrosity" TheEssexCountyExpress 2)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdVictoryPoints = Just 1
    }

grapplingHorror :: CardDef
grapplingHorror =
  (enemy "02182" "Grappling Horror" TheEssexCountyExpress 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
