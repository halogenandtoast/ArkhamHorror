module Arkham.Homebrew.DarkMatter.Locations.PalaceGates (palaceGates) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (flipToOtherSide)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype PalaceGates = PalaceGates LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The [[Carcosa]] face of Hidden Passage.
palaceGates :: LocationCard PalaceGates
palaceGates = locationWith PalaceGates Cards.palaceGates 2 (PerPlayer 1) (canBeFlippedL .~ True)

-- | "You cannot resign while Palace Gates is in play."
instance HasModifiersFor PalaceGates where
  getModifiersFor (PalaceGates a) = modifySelect a Anyone [CannotTakeAction $ IsAction #resign]

instance RunMessage PalaceGates where
  runMessage msg l@(PalaceGates attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      flipToOtherSide attrs
      pure l
    _ -> PalaceGates <$> liftRunMessage msg attrs
