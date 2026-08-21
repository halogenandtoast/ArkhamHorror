module Arkham.Treachery.Cards.ChildrenOfBlood.BloodBlight.GrislyCompulsion (grislyCompulsion) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.BloodBlight qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GrislyCompulsion = GrislyCompulsion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyCompulsion :: TreacheryCard GrislyCompulsion
grislyCompulsion = treachery GrislyCompulsion Cards.grislyCompulsion

instance RunMessage GrislyCompulsion where
  runMessage msg t@(GrislyCompulsion attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> GrislyCompulsion <$> liftRunMessage msg attrs
