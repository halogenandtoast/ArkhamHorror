module Arkham.Location.Cards.TombOfTheAncients (tombOfTheAncients) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TombOfTheAncients = TombOfTheAncients LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tombOfTheAncients :: LocationCard TombOfTheAncients
tombOfTheAncients = location TombOfTheAncients Cards.tombOfTheAncients 4 (PerPlayer 1)

instance HasAbilities TombOfTheAncients where
  getAbilities (TombOfTheAncients attrs) =
    extendRevealed attrs []

instance RunMessage TombOfTheAncients where
  runMessage msg (TombOfTheAncients attrs) = runQueueT $ case msg of
    _ -> TombOfTheAncients <$> liftRunMessage msg attrs
