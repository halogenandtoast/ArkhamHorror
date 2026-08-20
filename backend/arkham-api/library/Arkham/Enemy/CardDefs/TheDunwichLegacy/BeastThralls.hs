module Arkham.Enemy.CardDefs.TheDunwichLegacy.BeastThralls where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

avianThrall :: CardDef
avianThrall =
  (enemy "02094" "Avian Thrall" BeastThralls 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Creature, Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

lupineThrall :: CardDef
lupineThrall =
  (enemy "02095" "Lupine Thrall" BeastThralls 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }
