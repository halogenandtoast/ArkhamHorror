module Arkham.Event.Events.LookWhatIFound (lookWhatIFound) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype LookWhatIFound = LookWhatIFound EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lookWhatIFound :: EventCard LookWhatIFound
lookWhatIFound = event LookWhatIFound Cards.lookWhatIFound

instance RunMessage LookWhatIFound where
  runMessage msg e@(LookWhatIFound attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      discoverAtYourLocation NotInvestigate iid attrs 2
      pure e
    _ -> LookWhatIFound <$> liftRunMessage msg attrs
