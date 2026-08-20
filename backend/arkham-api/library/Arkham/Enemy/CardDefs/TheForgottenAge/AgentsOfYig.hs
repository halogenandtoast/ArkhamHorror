module Arkham.Enemy.CardDefs.TheForgottenAge.AgentsOfYig where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

broodOfYig :: CardDef
broodOfYig =
  (enemy "04083" "Brood of Yig" AgentsOfYig 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = singleton Keyword.Hunter
    }

serpentFromYoth :: CardDef
serpentFromYoth =
  (enemy "04084" "Serpent from Yoth" AgentsOfYig 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdVictoryPoints = Just 1
    }
