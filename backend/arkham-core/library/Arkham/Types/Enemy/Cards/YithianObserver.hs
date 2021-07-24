module Arkham.Types.Enemy.Cards.YithianObserver
  ( YithianObserver(..)
  , yithianObserver
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Source

newtype YithianObserver = YithianObserver EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianObserver :: EnemyCard YithianObserver
yithianObserver = enemyWith
  YithianObserver
  Cards.yithianObserver
  (4, Static 4, 3)
  (1, 1)
  (preyL .~ FewestCards)

instance HasModifiersFor env YithianObserver

instance ActionRunner env => HasActions env YithianObserver where
  getActions i window (YithianObserver attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env YithianObserver where
  runMessage msg e@(YithianObserver attrs@EnemyAttrs {..}) = case msg of
    PerformEnemyAttack iid eid damageStrategy | eid == enemyId -> do
      cardCount' <- unCardCount <$> getCount iid
      if cardCount' == 0
        then e <$ push
          (InvestigatorAssignDamage
            iid
            (EnemySource enemyId)
            damageStrategy
            (enemyHealthDamage + 1)
            (enemySanityDamage + 1)
          )
        else e <$ pushAll
          [ RandomDiscard iid
          , InvestigatorAssignDamage
            iid
            (EnemySource enemyId)
            DamageAny
            enemyHealthDamage
            enemySanityDamage
          ]
    _ -> YithianObserver <$> runMessage msg attrs
