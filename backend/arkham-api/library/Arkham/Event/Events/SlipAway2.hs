module Arkham.Event.Events.SlipAway2 (slipAway2) where

import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Modifier

newtype SlipAway2 = SlipAway2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slipAway2 :: EventCard SlipAway2
slipAway2 = event SlipAway2 Cards.slipAway2

instance RunMessage SlipAway2 where
  runMessage msg e@(SlipAway2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (AddSkillValue #intellect)
      chooseEvadeEnemy sid iid attrs
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n | n >= 1 -> do
      getSkillTestTargetedEnemy >>= traverse_ \x ->
        whenMatch x NonEliteEnemy $ nextPhaseModifier #upkeep attrs x DoesNotReadyDuringUpkeep

      when (n >= 3) $ atEndOfTurn attrs iid $ returnToHand iid (toCardId attrs)
      pure e
    _ -> SlipAway2 <$> liftRunMessage msg attrs
