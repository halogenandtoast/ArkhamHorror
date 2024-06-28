module Arkham.Event.Cards.FendOff3 (fendOff3, FendOff3 (..)) where

import Arkham.Attack
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Helpers.Window (spawnedEnemy)
import Arkham.Placement
import Arkham.Projection

newtype FendOff3 = FendOff3 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fendOff3 :: EventCard FendOff3
fendOff3 = event FendOff3 Cards.fendOff3

instance HasModifiersFor FendOff3 where
  getModifiersFor (EnemyTarget eid) (FendOff3 attrs) = case attrs.placement of
    AttachedToEnemy eid' | eid == eid' -> modified attrs [CannotReady]
    _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage FendOff3 where
  runMessage msg e@(FendOff3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let enemy = spawnedEnemy attrs.windows
      card <- field EnemyCard enemy
      focusCards [card] \unfocus -> do
        push $ InitiateEnemyAttack $ (enemyAttack enemy attrs iid) {attackAfter = [DoStep 1 msg]}
        push unfocus
      pure e
    DoStep 1 (PlayThisEvent iid (is attrs -> True)) -> do
      let enemy = spawnedEnemy attrs.windows
      push $ EnemyEvaded iid enemy
      push $ PlaceEvent iid attrs.id (AttachedToEnemy enemy)
      pure e
    _ -> FendOff3 <$> lift (runMessage msg attrs)
