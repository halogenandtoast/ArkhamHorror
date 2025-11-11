module Arkham.Treachery.Cards.ArcaneSpotlightB (arcaneSpotlightB) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ArcaneSpotlightB = ArcaneSpotlightB TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneSpotlightB :: TreacheryCard ArcaneSpotlightB
arcaneSpotlightB = treachery ArcaneSpotlightB Cards.arcaneSpotlightB

instance RunMessage ArcaneSpotlightB where
  runMessage msg t@(ArcaneSpotlightB attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ArcaneSpotlightB <$> liftRunMessage msg attrs
