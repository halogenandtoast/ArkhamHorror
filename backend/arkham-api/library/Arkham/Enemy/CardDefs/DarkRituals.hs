module Arkham.Enemy.CardDefs.DarkRituals where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

cultistOfTheEnclave :: CardDef
cultistOfTheEnclave =
  (enemy "84036" "Cultist of the Enclave" DarkRituals 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

dimensionalShambler :: CardDef
dimensionalShambler =
  (enemy "84035" "Dimensional Shambler" DarkRituals 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Monster, Extradimensional, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 2
    }
