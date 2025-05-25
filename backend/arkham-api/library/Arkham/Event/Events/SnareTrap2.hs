module Arkham.Event.Events.SnareTrap2 (snareTrap2) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Enemy
import Arkham.Helpers.Investigator
import Arkham.Helpers.Window (getEnemies)
import Arkham.Matcher
import Arkham.Window (getBatchId)

newtype SnareTrap2 = SnareTrap2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snareTrap2 :: EventCard SnareTrap2
snareTrap2 = event SnareTrap2 Cards.snareTrap2

instance HasAbilities SnareTrap2 where
  getAbilities (SnareTrap2 a) = case a.attachedTo of
    Just (LocationTarget lid) ->
      [mkAbility a 1 $ forced $ EnemyEnters #after (LocationWithId lid) NonEliteEnemy]
    Just (EnemyTarget eid) -> [mkAbility a 2 $ forced $ EnemyWouldReady #when $ EnemyWithId eid]
    _ -> []

instance RunMessage SnareTrap2 where
  runMessage msg e@(SnareTrap2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      lid <- getJustLocation iid
      place attrs (AttachedToLocation lid)
      pure e
    UseCardAbility _iid (isSource attrs -> True) 1 (getEnemies -> enemies) _ -> do
      for_ enemies \enemyId -> do
        exhaustThis enemyId
        disengageEnemyFromAll enemyId
        place attrs (AttachedToEnemy enemyId)
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 (getBatchId -> batchId) _ -> do
      cancelBatch batchId
      toDiscardBy attrs.owner (attrs.ability 2) attrs
      pure e
    _ -> SnareTrap2 <$> liftRunMessage msg attrs
