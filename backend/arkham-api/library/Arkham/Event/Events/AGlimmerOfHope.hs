module Arkham.Event.Events.AGlimmerOfHope (aGlimmerOfHope) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype AGlimmerOfHope = AGlimmerOfHope EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aGlimmerOfHope :: EventCard AGlimmerOfHope
aGlimmerOfHope = eventWith AGlimmerOfHope Cards.aGlimmerOfHope (afterPlayL .~ ReturnThisToHand)

instance RunMessage AGlimmerOfHope where
  runMessage msg e@(AGlimmerOfHope attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      discards <- select $ inDiscardOf iid <> basic "A Glimmer of Hope"
      unless (null discards) $ addToHand iid discards
      pure e
    _ -> AGlimmerOfHope <$> liftRunMessage msg attrs
