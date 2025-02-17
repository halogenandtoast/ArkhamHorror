module Arkham.Event.Events.UnconventionalMethod (unconventionalMethod) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier
import Arkham.Zone

newtype UnconventionalMethod = UnconventionalMethod EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unconventionalMethod :: EventCard UnconventionalMethod
unconventionalMethod = event UnconventionalMethod Cards.unconventionalMethod

instance RunMessage UnconventionalMethod where
  runMessage msg e@(UnconventionalMethod attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      case attrs.payment.discards of
        [(_zone, discardedCard)] -> do
          let n = discardedCard.printedCost
          sid <- getRandom
          when (n > 0) $ skillTestModifier sid attrs iid $ SkillModifier #intellect n
          investigate sid iid attrs
        _ -> error $ "Invalid choice: " <> show attrs.payment
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      case attrs.payment.discards of
        [(zone, _discardedCard)] -> do
          when (zone == FromPlay) do
            withSkillTest \sid -> do
              priority $ skillTestModifier sid attrs iid (DiscoveredClues 1)
        _ -> pure ()
      pure e
    _ -> UnconventionalMethod <$> liftRunMessage msg attrs
