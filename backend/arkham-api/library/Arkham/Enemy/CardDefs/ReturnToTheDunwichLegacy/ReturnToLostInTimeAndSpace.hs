module Arkham.Enemy.CardDefs.ReturnToTheDunwichLegacy.ReturnToLostInTimeAndSpace where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

sethBishopThrallOfYogSothoth :: CardDef
sethBishopThrallOfYogSothoth =
  (enemy "51056" ("Seth Bishop" <:> "Thrall of Yog-Sothoth") ReturnToLostInTimeAndSpace 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 5
    , cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Humanoid, Monster, Abomination, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    }
