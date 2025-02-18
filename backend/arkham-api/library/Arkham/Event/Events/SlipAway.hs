module Arkham.Event.Events.SlipAway (slipAway) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType

newtype SlipAway = SlipAway EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slipAway :: EventCard SlipAway
slipAway = event SlipAway Cards.slipAway

instance RunMessage SlipAway where
  runMessage msg e@(SlipAway attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (AddSkillValue SkillAgility)
      chooseEvadeEnemy sid iid attrs
      pure e
    PassedThisSkillTestBy _ (isSource attrs -> True) n | n >= 2 -> do
      getSkillTestTargetedEnemy >>= traverse_ \x ->
        whenMatch x NonEliteEnemy $ nextPhaseModifier #upkeep attrs x DoesNotReadyDuringUpkeep
      pure e
    _ -> SlipAway <$> liftRunMessage msg attrs
