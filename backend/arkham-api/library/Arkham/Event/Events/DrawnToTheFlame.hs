module Arkham.Event.Events.DrawnToTheFlame (drawnToTheFlame) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype DrawnToTheFlame = DrawnToTheFlame EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawnToTheFlame :: EventCard DrawnToTheFlame
drawnToTheFlame = event DrawnToTheFlame Cards.drawnToTheFlame

instance RunMessage DrawnToTheFlame where
  runMessage msg e@(DrawnToTheFlame attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      drawEncounterCard iid attrs
      discoverAtYourLocation NotInvestigate iid attrs 2
      pure e
    _ -> DrawnToTheFlame <$> liftRunMessage msg attrs
