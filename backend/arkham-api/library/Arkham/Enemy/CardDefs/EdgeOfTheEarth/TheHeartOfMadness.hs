module Arkham.Enemy.CardDefs.EdgeOfTheEarth.TheHeartOfMadness where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

protoplasmicMass :: CardDef
protoplasmicMass =
  (enemy "08669" "Protoplasmic Mass" TheGreatSeal 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 6
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Shoggoth]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

theNamelessMadness :: CardDef
theNamelessMadness =
  (enemy "08679" "The Nameless Madness" StirringInTheDeep 15)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fightPerInvestigator 1
    , cdEvade = evadePerInvestigator 1
    , cdCardTraits = setFromList [AncientOne, Eidolon, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Massive, Keyword.Retaliate]
    }

unsealedPhantasm :: CardDef
unsealedPhantasm =
  (enemy "08680" "Unsealed Phantasm" StirringInTheDeep 2)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdEvade = evade 4
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Eidolon, Shoggoth]
    , cdVictoryPoints = Just 1
    }
