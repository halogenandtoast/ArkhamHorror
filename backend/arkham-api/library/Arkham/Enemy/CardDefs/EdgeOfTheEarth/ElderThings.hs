module Arkham.Enemy.CardDefs.EdgeOfTheEarth.ElderThings where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

elderThingScavenger :: CardDef
elderThingScavenger =
  (enemy "08695" "Elder Thing Scavenger" ElderThings 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, ElderThing]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

guardianElderThing :: CardDef
guardianElderThing =
  (enemy "08696" "Guardian Elder Thing" ElderThings 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, ElderThing]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
