module Arkham.Location.Cards.HazuthKleg (hazuthKleg, HazuthKleg (..)) where

import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype HazuthKleg = HazuthKleg LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hazuthKleg :: LocationCard HazuthKleg
hazuthKleg = location HazuthKleg Cards.hazuthKleg 4 (PerPlayer 1)

instance HasAbilities HazuthKleg where
  getAbilities (HazuthKleg attrs) = veiled attrs []

instance RunMessage HazuthKleg where
  runMessage msg (HazuthKleg attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.unattainableDesires
      pure . HazuthKleg $ attrs & canBeFlippedL .~ False
    _ -> HazuthKleg <$> runMessage msg attrs
