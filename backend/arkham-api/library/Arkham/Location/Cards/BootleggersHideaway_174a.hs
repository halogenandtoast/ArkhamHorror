module Arkham.Location.Cards.BootleggersHideaway_174a
  ( bootleggersHideaway_174a
  , BootleggersHideaway_174a(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BootleggersHideaway_174a = BootleggersHideaway_174a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bootleggersHideaway_174a :: LocationCard BootleggersHideaway_174a
bootleggersHideaway_174a = location BootleggersHideaway_174a Cards.bootleggersHideaway_174a 4 (PerPlayer 1)

instance HasAbilities BootleggersHideaway_174a where
  getAbilities (BootleggersHideaway_174a attrs) =
    extendRevealed attrs []

instance RunMessage BootleggersHideaway_174a where
  runMessage msg (BootleggersHideaway_174a attrs) = runQueueT $ case msg of
    _ -> BootleggersHideaway_174a <$> liftRunMessage msg attrs
