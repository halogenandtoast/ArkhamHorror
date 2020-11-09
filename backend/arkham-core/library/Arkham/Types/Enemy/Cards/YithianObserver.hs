{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.YithianObserver where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype YithianObserver = YithianObserver Attrs
  deriving newtype (Show, ToJSON, FromJSON)

yithianObserver :: EnemyId -> YithianObserver
yithianObserver uuid =
  YithianObserver
    $ baseAttrs uuid "01177"
    $ (healthDamage .~ 1)
    . (sanityDamage .~ 1)
    . (fight .~ 4)
    . (health .~ Static 4)
    . (evade .~ 3)
    . (prey .~ FewestCards)

instance HasModifiersFor env YithianObserver where
  getModifiersFor _ _ _ = pure []

instance HasModifiers env YithianObserver where
  getModifiers _ (YithianObserver Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env YithianObserver where
  getActions i window (YithianObserver attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env YithianObserver where
  runMessage msg e@(YithianObserver attrs@Attrs {..}) = case msg of
    PerformEnemyAttack iid eid | eid == enemyId -> do
      cardCount' <- unCardCount <$> asks (getCount iid)
      if cardCount' == 0
        then e <$ unshiftMessage
          (InvestigatorAssignDamage
            iid
            (EnemySource enemyId)
            (enemyHealthDamage + 1)
            (enemySanityDamage + 1)
          )
        else e <$ unshiftMessages
          [ RandomDiscard iid
          , InvestigatorAssignDamage
            iid
            (EnemySource enemyId)
            enemyHealthDamage
            enemySanityDamage
          ]
    _ -> YithianObserver <$> runMessage msg attrs
