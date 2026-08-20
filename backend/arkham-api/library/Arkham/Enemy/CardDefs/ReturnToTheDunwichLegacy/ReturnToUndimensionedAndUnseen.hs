module Arkham.Enemy.CardDefs.ReturnToTheDunwichLegacy.ReturnToUndimensionedAndUnseen where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

broodOfYogSothothAmorphousTerror :: CardDef
broodOfYogSothothAmorphousTerror =
  (enemy "51045" ("Brood of Yog-Sothoth" <:> "Amorphous Terror") ReturnToUndimensionedAndUnseen 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 6
    , cdEvade = evade 3
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

broodOfYogSothothChargingBeast :: CardDef
broodOfYogSothothChargingBeast =
  (enemy "51042" ("Brood of Yog-Sothoth" <:> "Charging Beast") ReturnToUndimensionedAndUnseen 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 4
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

broodOfYogSothothSwellingDevourer :: CardDef
broodOfYogSothothSwellingDevourer =
  (enemy "51043" ("Brood of Yog-Sothoth" <:> "Swelling Devourer") ReturnToUndimensionedAndUnseen 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 6
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

broodOfYogSothothThrashingSpawn :: CardDef
broodOfYogSothothThrashingSpawn =
  (enemy "51044" ("Brood of Yog-Sothoth" <:> "Thrashing Spawn") ReturnToUndimensionedAndUnseen 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 7
    , cdEvade = evade 3
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }
