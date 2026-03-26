module Arkham.Event.Events.QuickExit2 (quickExit2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTestTargetedEnemy)
import Arkham.Matcher
import Arkham.Modifier

newtype QuickExit2 = QuickExit2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickExit2 :: EventCard QuickExit2
quickExit2 = event QuickExit2 Cards.quickExit2

instance RunMessage QuickExit2 where
  runMessage msg e@(QuickExit2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (AddSkillValue #willpower)
      chooseEvadeEnemy sid iid attrs
      pure e
    PassedThisSkillTest _iid (isSource attrs -> True) -> do
      withSkillTestTargetedEnemy \enemy -> do
        whenMatch enemy NonEliteEnemy do
          nextPhaseModifier #upkeep attrs enemy DoesNotReadyDuringUpkeep
      pure e
    _ -> QuickExit2 <$> liftRunMessage msg attrs
