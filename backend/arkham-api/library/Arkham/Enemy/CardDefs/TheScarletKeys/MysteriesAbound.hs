module Arkham.Enemy.CardDefs.TheScarletKeys.MysteriesAbound where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

coterieEnvoy :: CardDef
coterieEnvoy =
  (enemy "09720" "Coterie Envoy" MysteriesAbound 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords =
        setFromList
          [ Keyword.Aloof
          , Keyword.Patrol
              $ LocationWithConcealedCard
              <> not_ (LocationWithEnemy $ EnemyIs "09720" <> not_ ThatEnemy)
          ]
    }
