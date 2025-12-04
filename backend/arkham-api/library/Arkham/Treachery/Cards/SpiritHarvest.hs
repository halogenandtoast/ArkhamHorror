module Arkham.Treachery.Cards.SpiritHarvest (spiritHarvest) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SpiritHarvest = SpiritHarvest TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritHarvest :: TreacheryCard SpiritHarvest
spiritHarvest = treachery SpiritHarvest Cards.spiritHarvest

instance RunMessage SpiritHarvest where
  runMessage msg t@(SpiritHarvest attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> SpiritHarvest <$> liftRunMessage msg attrs
