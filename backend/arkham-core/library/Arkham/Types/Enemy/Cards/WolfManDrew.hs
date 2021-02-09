module Arkham.Types.Enemy.Cards.WolfManDrew where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype WolfManDrew = WolfManDrew EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wolfManDrew :: EnemyId -> WolfManDrew
wolfManDrew uuid =
  WolfManDrew
    $ baseAttrs uuid "01137"
    $ (healthDamageL .~ 2)
    . (fightL .~ 4)
    . (healthL .~ Static 4)
    . (evadeL .~ 2)
    . (uniqueL .~ True)

instance HasModifiersFor env WolfManDrew where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env WolfManDrew where
  getActions i window (WolfManDrew attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env WolfManDrew where
  runMessage msg e@(WolfManDrew attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) eid (LocationWithTitle "Downtown")
    PerformEnemyAttack _ eid | eid == enemyId ->
      WolfManDrew <$> runMessage msg (attrs & damageL %~ max 0 . subtract 1)
    _ -> WolfManDrew <$> runMessage msg attrs
