module Arkham.Homebrew.DarkMatter.Locations.HiddenPassage (hiddenPassage) where

import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (flipToOtherSide)
import Arkham.Location.Import.Lifted

newtype HiddenPassage = HiddenPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | No printed ability; it is the [[Surface]]/[[Cave]] junction of the map.
hiddenPassage :: LocationCard HiddenPassage
hiddenPassage = locationWith HiddenPassage Cards.hiddenPassage 2 (Static 1) (canBeFlippedL .~ True)

instance RunMessage HiddenPassage where
  runMessage msg l@(HiddenPassage attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      flipToOtherSide attrs
      pure l
    _ -> HiddenPassage <$> liftRunMessage msg attrs
