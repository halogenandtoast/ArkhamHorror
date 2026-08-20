module Arkham.Enemy.CardDefs.NightOfTheZealot.AgentsOfShubNiggurath where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

goatSpawn :: CardDef
goatSpawn =
  (enemy "01180" "Goat Spawn" AgentsOfShubNiggurath 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

relentlessDarkYoung :: CardDef
relentlessDarkYoung =
  (enemy "01179" "Relentless Dark Young" AgentsOfShubNiggurath 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, DarkYoung]
    , cdVictoryPoints = Just 1
    }
