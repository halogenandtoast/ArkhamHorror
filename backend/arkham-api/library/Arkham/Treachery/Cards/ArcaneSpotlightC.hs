module Arkham.Treachery.Cards.ArcaneSpotlightC (arcaneSpotlightC) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ArcaneSpotlightC = ArcaneSpotlightC TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneSpotlightC :: TreacheryCard ArcaneSpotlightC
arcaneSpotlightC = treachery ArcaneSpotlightC Cards.arcaneSpotlightC

instance RunMessage ArcaneSpotlightC where
  runMessage msg t@(ArcaneSpotlightC attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ArcaneSpotlightC <$> liftRunMessage msg attrs
