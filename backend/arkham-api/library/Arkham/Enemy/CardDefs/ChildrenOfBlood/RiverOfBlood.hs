module Arkham.Enemy.CardDefs.ChildrenOfBlood.RiverOfBlood where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

juliaSternOnTheRun :: CardDef
juliaSternOnTheRun =
  (enemy "13024" ("Julia Stern" <:> "On the Run") RiverOfBlood 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdHealth = health 6
    , cdEvade = evade 4
    , cdCardTraits = setFromList [Humanoid, Monster, Elite]
    , cdKeywords =
        setFromList
          [Keyword.Elusive, Keyword.Patrol (LocationWithEnemy $ EnemyWithTrait Civilian), Keyword.Predator]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

juliaSternStalkingTheStreets :: CardDef
juliaSternStalkingTheStreets =
  (enemy "13025" ("Julia Stern" <:> "Stalking the Streets") RiverOfBlood 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdHealth = health 8
    , cdEvade = evade 4
    , cdCardTraits = setFromList [Humanoid, Monster, Elite]
    , cdKeywords =
        setFromList
          [ Keyword.Elusive
          , Keyword.Patrol (LocationWithEnemy $ EnemyWithTrait Civilian)
          , Keyword.Predator
          , Keyword.Retaliate
          ]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

juliaSternPreyingUponArkham :: CardDef
juliaSternPreyingUponArkham =
  (enemy "13026" ("Julia Stern" <:> "Preying Upon Arkham") RiverOfBlood 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdHealth = health 8
    , cdEvade = evade 4
    , cdCardTraits = setFromList [Humanoid, Monster, Elite]
    , cdKeywords =
        setFromList
          [ Keyword.Elusive
          , Keyword.Patrol (LocationWithEnemy $ EnemyWithTrait Civilian)
          , Keyword.Predator
          , Keyword.Retaliate
          ]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

waterfrontCivilian :: CardDef
waterfrontCivilian =
  (enemy "13027" "Waterfront Civilian" RiverOfBlood 4)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdHealth = health 1
    , cdEvade = evade 1
    , cdCardTraits = setFromList [Humanoid, Civilian]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Doomed]
    , cdVictoryPoints = Just 0
    }
