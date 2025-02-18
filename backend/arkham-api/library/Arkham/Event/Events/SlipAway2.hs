module Arkham.Event.Events.SlipAway2 (slipAway2, slipAway2Effect) where

import Arkham.Card
import Arkham.Effect.Import
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
      skillTestModifier sid attrs iid (AddSkillValue #agility)
      chooseEvadeEnemy sid iid attrs
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n | n >= 1 -> do
      getSkillTestTargetedEnemy >>= traverse_ \x ->
        whenMatch x NonEliteEnemy $ nextPhaseModifier #upkeep attrs x DoesNotReadyDuringUpkeep

      when (n >= 3) do
        createCardEffect Cards.slipAway2 (Just $ EffectMetaTarget (toTarget $ toCardId attrs)) attrs iid
      pure e
    _ -> SlipAway2 <$> liftRunMessage msg attrs

newtype SlipAway2Effect = SlipAway2Effect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slipAway2Effect :: EffectArgs -> SlipAway2Effect
slipAway2Effect = cardEffect SlipAway2Effect Cards.slipAway2

instance RunMessage SlipAway2Effect where
  runMessage msg e@(SlipAway2Effect attrs) = runQueueT $ case msg of
    EndTurn iid | isTarget iid attrs.target -> do
      case attrs.meta of
        Just (EffectMetaTarget (CardIdTarget cardId)) -> do
          disable attrs
          returnToHand iid (toTarget cardId)
        _ -> error "invalid meta target"
      pure e
    _ -> SlipAway2Effect <$> liftRunMessage msg attrs
