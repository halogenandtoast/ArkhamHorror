module Arkham.Location.Cards.TwistingCatwalks (twistingCatwalks) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TwistingCatwalks = TwistingCatwalks LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistingCatwalks :: LocationCard TwistingCatwalks
twistingCatwalks = location TwistingCatwalks Cards.twistingCatwalks 3 (Static 1)

-- TODO: abilities

instance RunMessage TwistingCatwalks where
  runMessage msg (TwistingCatwalks attrs) = runQueueT $ TwistingCatwalks <$> liftRunMessage msg attrs
