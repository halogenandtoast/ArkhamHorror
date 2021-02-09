module Arkham.Types.Enemy.Cards.BillyCooper where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Trait

newtype BillyCooper = BillyCooper EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

billyCooper :: EnemyId -> BillyCooper
billyCooper uuid =
  BillyCooper
    $ baseAttrs uuid "50045"
    $ (healthDamageL .~ 2)
    . (fightL .~ 5)
    . (healthL .~ Static 4)
    . (evadeL .~ 2)
    . (uniqueL .~ True)

instance HasModifiersFor env BillyCooper where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env BillyCooper where
  getActions iid window (BillyCooper attrs) = getActions iid window attrs

instance (EnemyRunner env) => RunMessage env BillyCooper where
  runMessage msg e@(BillyCooper attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) eid (LocationWithTitle "Easttown")
    After (EnemyDefeated _ _ lid _ _ traits)
      | lid == enemyLocation && Monster `elem` traits -> e
      <$ unshiftMessage (AddToVictory $ toTarget attrs)
    _ -> BillyCooper <$> runMessage msg attrs
