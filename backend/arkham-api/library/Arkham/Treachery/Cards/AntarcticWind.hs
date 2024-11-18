module Arkham.Treachery.Cards.AntarcticWind (antarcticWind, AntarcticWind (..)) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AntarcticWind = AntarcticWind TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

antarcticWind :: TreacheryCard AntarcticWind
antarcticWind = treachery AntarcticWind Cards.antarcticWind

instance RunMessage AntarcticWind where
  runMessage msg t@(AntarcticWind attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> AntarcticWind <$> liftRunMessage msg attrs
