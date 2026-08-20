{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

shadowAgents :: CardDef
shadowAgents =
  (weakness "07011" "Shadow Agents")
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 5
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = singleton Keyword.Hunter
    }

accursedFollower :: CardDef
accursedFollower =
  (basicWeakness "07038" "Accursed Follower")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist, Cursed]
    , cdKeywords = singleton Keyword.Aloof
    }
