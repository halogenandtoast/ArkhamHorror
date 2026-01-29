module Arkham.Event.Events.ExistentialRiddle1 (existentialRiddle1) where

import Arkham.Calculation
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (withSkillTestTargetedEnemy)
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher hiding (EnemyEvaded)

newtype ExistentialRiddle1 = ExistentialRiddle1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

existentialRiddle1 :: EventCard ExistentialRiddle1
existentialRiddle1 = event ExistentialRiddle1 Cards.existentialRiddle1

instance HasModifiersFor ExistentialRiddle1 where
  getModifiersFor (ExistentialRiddle1 a) = for_ a.attachedTo \target ->
    modified_ a target [AddKeyword Aloof]

instance RunMessage ExistentialRiddle1 where
  runMessage msg e@(ExistentialRiddle1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ enemyAtLocationWith iid <> NonEliteEnemy <> canParleyEnemy iid
      pure e
    HandleTargetChoice iid (is attrs -> True) (EnemyTarget eid) -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid attrs eid [#willpower, #intellect]
        $ MinCalculation (Fixed 0) (SubtractCalculation (Fixed 8) (InvestigatorHandLengthCalculation iid))
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      withSkillTestTargetedEnemy \eid -> do
        automaticallyEvadeEnemy iid eid
        place attrs $ AttachedToEnemy eid
      pure e
    _ -> ExistentialRiddle1 <$> liftRunMessage msg attrs
