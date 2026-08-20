module Arkham.Enemy.CardDefs.EdgeOfTheEarth.AgentsOfTheUnknown where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

primordialEvil :: CardDef
primordialEvil =
  (enemy "08687" "Primordial Evil" AgentsOfTheUnknown 2)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Eidolon]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }
