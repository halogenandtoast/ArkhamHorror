module Arkham.Enemy.CardDefs.TheForgottenAge.HeartOfTheElders where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

apexStrangleweed :: CardDef
apexStrangleweed =
  (enemy "04219" "Apex Strangleweed" PillarsOfJudgement 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 6
    , cdCardTraits = setFromList [Creature, Flora]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

basilisk :: CardDef
basilisk =
  (enemy "04220" "Basilisk" PillarsOfJudgement 2)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Serpent]
    , cdKeywords = singleton Keyword.Hunter
    , cdVengeancePoints = Just 2
    }

featheredSerpent :: CardDef
featheredSerpent =
  (enemy "53047" "Feathered Serpent" ReturnToPillarsOfJudgement 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fightX
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Serpent]
    , cdVengeancePoints = Just 1
    }

theWingedSerpent :: CardDef
theWingedSerpent =
  doubleSided "04209"
    $ (enemy "04209b" ("The Winged Serpent" <:> "The Wrath of Yig") PillarsOfJudgement 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 8
      , cdEvade = evade 5
      , cdCardTraits = setFromList [Monster, Serpent, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Massive]
      }

theWingedSerpentTheFuryOfYig :: CardDef
theWingedSerpentTheFuryOfYig =
  doubleSided "53046"
    $ (enemy "53046b" ("The Winged Serpent" <:> "The Fury of Yig") PillarsOfJudgement 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdCardTraits = setFromList [Monster, Serpent, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      }
