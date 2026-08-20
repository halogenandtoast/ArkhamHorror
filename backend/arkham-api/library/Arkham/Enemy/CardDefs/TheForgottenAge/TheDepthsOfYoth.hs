module Arkham.Enemy.CardDefs.TheForgottenAge.TheDepthsOfYoth where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

eaterOfTheDepths :: CardDef
eaterOfTheDepths =
  (enemy "04298" "Eater of the Depths" TheDepthsOfYoth 1)
    { cdHealthDamage = healthDamage 3
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdEvade = evadeX
    , cdHealth = health 6
    , cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 2
    }

pitWarden :: CardDef
pitWarden =
  (enemy "04297" "Pit Warden" TheDepthsOfYoth 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdVengeancePoints = Just 1
    , cdKeywords = singleton Keyword.Hunter
    }

yig :: CardDef
yig =
  unique
    $ (enemy "04296" ("Yig" <:> "The Father of Serpents") TheDepthsOfYoth 1)
      { cdHealthDamage = healthDamage 3
      , cdSanityDamage = sanityDamage 3
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 6
      , cdCardTraits = setFromList [AncientOne, Serpent, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      , cdVictoryPoints = Just 5
      }
