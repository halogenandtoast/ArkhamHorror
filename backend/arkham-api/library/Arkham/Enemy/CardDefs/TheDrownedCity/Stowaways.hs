module Arkham.Enemy.CardDefs.TheDrownedCity.Stowaways where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

stowawayDrone :: CardDef
stowawayDrone =
  (enemy "11721" "Stowaway Drone" Stowaways 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Stowaway]
    , cdKeywords =
        setFromList
          [ Keyword.Aloof
          , Keyword.Patrol $ LocationWithEnemy (not_ ThatEnemy <> EnemyWithoutDoom)
          ]
    }
