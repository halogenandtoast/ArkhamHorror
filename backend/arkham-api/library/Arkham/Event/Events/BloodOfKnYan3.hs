module Arkham.Event.Events.BloodOfKnYan3 (bloodOfKnYan3) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.Slot
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype BloodOfKnYan3 = BloodOfKnYan3 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodOfKnYan3 :: EventCard BloodOfKnYan3
bloodOfKnYan3 = event BloodOfKnYan3 Cards.bloodOfKnYan3

instance HasModifiersFor BloodOfKnYan3 where
  getModifiersFor (BloodOfKnYan3 a) = do
    slots <- fieldMap InvestigatorSlots (count isEmptySlot . findWithDefault [] HandSlot) a.controller
    modifiedWhen_ a (slots > 0) a.controller [AnySkillValue (slots * 2), DamageDealt slots]

instance RunMessage BloodOfKnYan3 where
  runMessage msg e@(BloodOfKnYan3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      chooseOneM iid $ for_ [#willpower, #combat] \sType -> do
        skillLabeled sType $ chooseFightEnemyEdit sid iid attrs (withSkillType sType)
      pure e
    EnemyDefeated eid _ (isSource attrs -> True) _ -> do
      addToVictory eid
      addToVictory attrs
      pure e
    _ -> BloodOfKnYan3 <$> liftRunMessage msg attrs
