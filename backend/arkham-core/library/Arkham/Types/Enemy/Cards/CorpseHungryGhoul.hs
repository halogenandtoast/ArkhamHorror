module Arkham.Types.Enemy.Cards.CorpseHungryGhoul where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype CorpseHungryGhoul = CorpseHungryGhoul EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corpseHungryGhoul :: EnemyId -> CorpseHungryGhoul
corpseHungryGhoul uuid =
  CorpseHungryGhoul
    $ baseAttrs uuid "50022"
    $ (healthDamageL .~ 2)
    . (sanityDamageL .~ 2)
    . (fightL .~ 4)
    . (healthL .~ Static 3)
    . (evadeL .~ 3)

instance HasModifiersFor env CorpseHungryGhoul where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env CorpseHungryGhoul where
  getActions i window (CorpseHungryGhoul attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env CorpseHungryGhoul where
  runMessage msg e@(CorpseHungryGhoul attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) enemyId (LocationWithTitle "Bedroom")
    _ -> CorpseHungryGhoul <$> runMessage msg attrs
