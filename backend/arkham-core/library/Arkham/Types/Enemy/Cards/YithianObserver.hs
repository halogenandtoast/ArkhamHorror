module Arkham.Types.Enemy.Cards.YithianObserver where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype YithianObserver = YithianObserver EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianObserver :: EnemyId -> YithianObserver
yithianObserver uuid =
  YithianObserver
    $ baseAttrs uuid "01177"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 4)
    . (healthL .~ Static 4)
    . (evadeL .~ 3)
    . (preyL .~ FewestCards)

instance HasModifiersFor env YithianObserver where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env YithianObserver where
  getActions i window (YithianObserver attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env YithianObserver where
  runMessage msg e@(YithianObserver attrs@EnemyAttrs {..}) = case msg of
    PerformEnemyAttack iid eid | eid == enemyId -> do
      cardCount' <- unCardCount <$> getCount iid
      if cardCount' == 0
        then e <$ unshiftMessage
          (InvestigatorAssignDamage
            iid
            (EnemySource enemyId)
            DamageAny
            (enemyHealthDamage + 1)
            (enemySanityDamage + 1)
          )
        else e <$ unshiftMessages
          [ RandomDiscard iid
          , InvestigatorAssignDamage
            iid
            (EnemySource enemyId)
            DamageAny
            enemyHealthDamage
            enemySanityDamage
          ]
    _ -> YithianObserver <$> runMessage msg attrs
