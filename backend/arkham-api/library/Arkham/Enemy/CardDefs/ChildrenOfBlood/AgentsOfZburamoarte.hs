module Arkham.Enemy.CardDefs.ChildrenOfBlood.AgentsOfZburamoarte where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

spawnOfZburamoarte :: CardDef
spawnOfZburamoarte =
  (enemy "13097" "Spawn of Zburamoarte" AgentsOfZburamoarte 3)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdHealth = health 5
    , cdEvade = evade 3
    , cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Predator]
    , cdVictoryPoints = Just 1
    }
