module Arkham.Enemy.CardDefs.TheDunwichLegacy.Whippoorwills where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

whippoorwill :: CardDef
whippoorwill =
  (enemy "02090" "Whippoorwill" Whippoorwills 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }
