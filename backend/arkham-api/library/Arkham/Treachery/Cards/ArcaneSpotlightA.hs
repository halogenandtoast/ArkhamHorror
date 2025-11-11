module Arkham.Treachery.Cards.ArcaneSpotlightA (arcaneSpotlightA) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ArcaneSpotlightA = ArcaneSpotlightA TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneSpotlightA :: TreacheryCard ArcaneSpotlightA
arcaneSpotlightA = treachery ArcaneSpotlightA Cards.arcaneSpotlightA

instance RunMessage ArcaneSpotlightA where
  runMessage msg t@(ArcaneSpotlightA attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ArcaneSpotlightA <$> liftRunMessage msg attrs
