module Arkham.Enemy.CardDefs.TheDunwichLegacy.TheMiskatonicMuseum where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

huntingHorror :: CardDef
huntingHorror =
  ( enemy
      "02141"
      ("Hunting Horror" <:> "Spawned from the Void")
      TheMiskatonicMuseum
      1
  )
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }
