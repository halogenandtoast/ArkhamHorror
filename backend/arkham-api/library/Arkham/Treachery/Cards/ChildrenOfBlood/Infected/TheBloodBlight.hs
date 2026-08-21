module Arkham.Treachery.Cards.ChildrenOfBlood.Infected.TheBloodBlight (theBloodBlight) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.Infected qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheBloodBlight = TheBloodBlight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBloodBlight :: TreacheryCard TheBloodBlight
theBloodBlight = treachery TheBloodBlight Cards.theBloodBlight

instance RunMessage TheBloodBlight where
  runMessage msg t@(TheBloodBlight attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> TheBloodBlight <$> liftRunMessage msg attrs
