module Arkham.Enemy.CardDefs.TheDunwichLegacy.LostInTimeAndSpace where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

interstellarTraveler :: CardDef
interstellarTraveler =
  (enemy "02329" "Interstellar Traveler" LostInTimeAndSpace 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

yithianStarseeker :: CardDef
yithianStarseeker =
  (enemy "02330" "Yithian Starseeker" LostInTimeAndSpace 2)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 5
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

yogSothoth :: CardDef
yogSothoth =
  unique
    $ ( enemy
          "02323"
          ("Yog-Sothoth" <:> "The Lurker Beyond the Threshold")
          LostInTimeAndSpace
          1
      )
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 5
      , cdFight = fight 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords =
          setFromList
            [Keyword.Massive, Keyword.Hunter, Keyword.Retaliate]
      }
