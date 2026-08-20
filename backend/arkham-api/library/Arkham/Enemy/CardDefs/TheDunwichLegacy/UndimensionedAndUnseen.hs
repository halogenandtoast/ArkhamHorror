module Arkham.Enemy.CardDefs.TheDunwichLegacy.UndimensionedAndUnseen where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

broodOfYogSothoth :: CardDef
broodOfYogSothoth =
  (enemy "02255" "Brood of Yog-Sothoth" UndimensionedAndUnseen 5)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 6
    , cdEvade = evade 3
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }
