module Arkham.Enemy.CardDefs.ThePathToCarcosa.CultOfTheYellowSign where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

agentOfTheKing :: CardDef
agentOfTheKing =
  (enemy "03099" "Agent of the King" CultOfTheYellowSign 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

fanatic :: CardDef
fanatic =
  (enemy "03098" "Fanatic" CultOfTheYellowSign 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }
