module Arkham.Event.Events.GritYourTeeth (gritYourTeeth) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier

newtype GritYourTeeth = GritYourTeeth EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gritYourTeeth :: EventCard GritYourTeeth
gritYourTeeth = event GritYourTeeth Cards.gritYourTeeth

instance RunMessage GritYourTeeth where
  runMessage msg e@(GritYourTeeth attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      roundModifiers attrs iid [SkillModifier kind 1 | kind <- [minBound ..]]
      pure e
    _ -> GritYourTeeth <$> liftRunMessage msg attrs
