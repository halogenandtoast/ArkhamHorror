module Arkham.Treachery.Cards.TheMadnessWithin (theMadnessWithin) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheMadnessWithin = TheMadnessWithin TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMadnessWithin :: TreacheryCard TheMadnessWithin
theMadnessWithin = treachery TheMadnessWithin Cards.theMadnessWithin

instance RunMessage TheMadnessWithin where
  runMessage msg t@(TheMadnessWithin attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> TheMadnessWithin <$> liftRunMessage msg attrs
