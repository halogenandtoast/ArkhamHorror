module Arkham.Enemy.CardDefs.ReturnToTheDunwichLegacy.YogSothothsEmissaries where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

vassalOfTheLurker :: CardDef
vassalOfTheLurker =
  (enemy "51071" "Vassal of the Lurker" YogSothothsEmissaries 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }
