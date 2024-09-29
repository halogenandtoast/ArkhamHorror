module Arkham.Location.Cards.BootleggersHideaway_174b
  ( bootleggersHideaway_174b
  , BootleggersHideaway_174b(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BootleggersHideaway_174b = BootleggersHideaway_174b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bootleggersHideaway_174b :: LocationCard BootleggersHideaway_174b
bootleggersHideaway_174b = location BootleggersHideaway_174b Cards.bootleggersHideaway_174b 4 (PerPlayer 1)

instance HasAbilities BootleggersHideaway_174b where
  getAbilities (BootleggersHideaway_174b attrs) =
    extendRevealed attrs []

instance RunMessage BootleggersHideaway_174b where
  runMessage msg (BootleggersHideaway_174b attrs) = runQueueT $ case msg of
    _ -> BootleggersHideaway_174b <$> liftRunMessage msg attrs
