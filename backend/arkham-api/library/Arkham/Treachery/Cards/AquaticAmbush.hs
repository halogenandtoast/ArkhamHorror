module Arkham.Treachery.Cards.AquaticAmbush
  ( aquaticAmbush
  , AquaticAmbush(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AquaticAmbush = AquaticAmbush TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aquaticAmbush :: TreacheryCard AquaticAmbush
aquaticAmbush = treachery AquaticAmbush Cards.aquaticAmbush

instance RunMessage AquaticAmbush where
  runMessage msg t@(AquaticAmbush attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> AquaticAmbush <$> liftRunMessage msg attrs
