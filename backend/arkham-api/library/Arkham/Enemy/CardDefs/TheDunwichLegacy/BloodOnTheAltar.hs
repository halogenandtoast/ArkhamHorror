module Arkham.Enemy.CardDefs.TheDunwichLegacy.BloodOnTheAltar where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

servantOfManyMouths :: CardDef
servantOfManyMouths =
  (enemy "02224" "Servant of Many Mouths" BloodOnTheAltar 3)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 2
    , cdCardTraits = singleton Humanoid
    , cdKeywords = singleton Keyword.Retaliate
    }

silasBishop :: CardDef
silasBishop =
  unique
    $ (enemy "02216" ("Silas Bishop" <:> "Infused With Evil") BloodOnTheAltar 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 7
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = singleton Keyword.Massive
      , cdVictoryPoints = Just 2
      }
