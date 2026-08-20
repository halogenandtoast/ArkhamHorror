module Arkham.Enemy.CardDefs.TheDreamEaters.DarkSideOfTheMoon where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

catsFromSaturn :: CardDef
catsFromSaturn =
  (enemy "06228" "Cats from Saturn" DarkSideOfTheMoon 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Swarming (Static 0)]
    }

moonBeast :: CardDef
moonBeast =
  (enemy "06229" "Moon-Beast" DarkSideOfTheMoon 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 1
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Servitor]
    , cdKeywords = singleton Keyword.Retaliate
    , cdVictoryPoints = Just 1
    }

moonLizard :: CardDef
moonLizard =
  (enemy "06226" "Moon Lizard" DarkSideOfTheMoon 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fightX
    , cdEvade = evadeX
    , cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 2
    }

moonboundByakhee :: CardDef
moonboundByakhee =
  (enemy "06227" "Moonbound Byakhee" DarkSideOfTheMoon 2)
    { cdHealthDamage = healthDamage 3
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }
