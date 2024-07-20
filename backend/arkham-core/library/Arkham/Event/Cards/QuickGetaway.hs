module Arkham.Event.Cards.QuickGetaway (quickGetaway, QuickGetaway (..)) where

import Arkham.Attack.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (inSkillTest)
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Window (Window (..), WindowType (..))

newtype Meta = Meta {attackDetails :: Maybe EnemyAttackDetails}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype QuickGetaway = QuickGetaway (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickGetaway :: EventCard QuickGetaway
quickGetaway = event (QuickGetaway . (`with` Meta Nothing)) Cards.quickGetaway

instance RunMessage QuickGetaway where
  runMessage msg e@(QuickGetaway (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      needsToMoveAttack <- inSkillTest
      let details = getAttackDetails attrs.windows
          isEnemyAttackWindow = \case
            EnemyAttacksEvenIfCancelled details' -> details.enemy == details'.enemy
            _ -> False
      moveWithSkillTest \case
        PerformEnemyAttack eid -> eid == details.enemy
        After (PerformEnemyAttack eid) -> eid == details.enemy
        CheckWindow _ ws -> any (isEnemyAttackWindow . windowType) ws
        _ -> False

      push $ EvadeEnemy iid details.enemy (toSource attrs) Nothing #agility False
      pure . QuickGetaway $ attrs `with` Meta (Just details)
    PassedThisSkillTest _iid (isSource attrs -> True) -> do
      for_ (attackDetails meta) \details -> do
        when (attackCanBeCanceled details) do
          cancelAttack attrs details
      pure e
    _ -> QuickGetaway . (`with` meta) <$> liftRunMessage msg attrs
