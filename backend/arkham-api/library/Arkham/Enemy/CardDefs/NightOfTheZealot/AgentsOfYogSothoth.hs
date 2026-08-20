module Arkham.Enemy.CardDefs.NightOfTheZealot.AgentsOfYogSothoth where

import Arkham.Enemy.CardDefs.Import

yithianObserver :: CardDef
yithianObserver =
  (enemy "01177" "Yithian Observer" AgentsOfYogSothoth 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Yithian]
    , cdVictoryPoints = Just 1
    }
