module Arkham.Treachery.Cards.Snowfall (snowfall) where

import Arkham.Helpers.Investigator
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Snowfall = Snowfall TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snowfall :: TreacheryCard Snowfall
snowfall = treachery Snowfall Cards.snowfall

instance RunMessage Snowfall where
  runMessage msg t@(Snowfall attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \loc -> placeClues attrs loc 2
      pure t
    _ -> Snowfall <$> liftRunMessage msg attrs
