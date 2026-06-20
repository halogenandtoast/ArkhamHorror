module Arkham.Enemy.CardDefs.ByTheBook where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

mrGrey :: CardDef
mrGrey =
  unique
    $ doubleSided "90033a"
    $ (enemy "90033b" ("Mr. Grey" <:> "Corrupt Politician") ByTheBook 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      }
