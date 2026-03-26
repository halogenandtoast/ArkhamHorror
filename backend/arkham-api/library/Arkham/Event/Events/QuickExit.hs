module Arkham.Event.Events.QuickExit (quickExit) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTestTargetedEnemy)
import Arkham.Matcher
import Arkham.Modifier

newtype QuickExit = QuickExit EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickExit :: EventCard QuickExit
quickExit = event QuickExit Cards.quickExit

instance RunMessage QuickExit where
  runMessage msg e@(QuickExit attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (SkillModifier #agility 2)
      chooseEvadeEnemy sid iid attrs
      pure e
    PassedThisSkillTest _iid (isSource attrs -> True) -> do
      withSkillTestTargetedEnemy \enemy -> do
        whenMatch enemy NonEliteEnemy do
          nextPhaseModifier #upkeep attrs enemy DoesNotReadyDuringUpkeep
      pure e
    _ -> QuickExit <$> liftRunMessage msg attrs
