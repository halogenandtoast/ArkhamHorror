module Arkham.Enemy.CardDefs.TheDunwichLegacy.BishopsThralls where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

thrall :: CardDef
thrall =
  (enemy "02086" "Thrall" BishopsThralls 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

wizardOfYogSothoth :: CardDef
wizardOfYogSothoth =
  (enemy "02087" "Wizard of Yog-Sothoth" BishopsThralls 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Sorcerer]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }
