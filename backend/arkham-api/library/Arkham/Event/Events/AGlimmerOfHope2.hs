module Arkham.Event.Events.AGlimmerOfHope2 (aGlimmerOfHope2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype AGlimmerOfHope2 = AGlimmerOfHope2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aGlimmerOfHope2 :: EventCard AGlimmerOfHope2
aGlimmerOfHope2 = eventWith AGlimmerOfHope2 Cards.aGlimmerOfHope2 (afterPlayL .~ ReturnThisToHand)

instance RunMessage AGlimmerOfHope2 where
  runMessage msg e@(AGlimmerOfHope2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      discards <- select $ inDiscardOf iid <> basic "A Glimmer of Hope"
      unless (null discards) $ addToHand iid discards
      pure e
    _ -> AGlimmerOfHope2 <$> liftRunMessage msg attrs
