module Arkham.Types.Enemy.Cards.Mobster where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype Mobster = Mobster EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mobster :: EnemyId -> Mobster
mobster uuid =
  Mobster
    $ baseAttrs uuid "02098"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 0)
    . (fightL .~ 2)
    . (healthL .~ Static 2)
    . (evadeL .~ 2)

instance HasModifiersFor env Mobster where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Mobster where
  getActions i window (Mobster attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env Mobster where
  runMessage msg e@(Mobster attrs@EnemyAttrs {..}) = case msg of
    After (PerformEnemyAttack iid eid) | eid == enemyId ->
      e <$ unshiftMessage (SpendResources iid 1)
    _ -> Mobster <$> runMessage msg attrs
