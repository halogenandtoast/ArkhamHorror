module Arkham.Homebrew.DarkMatter.Locations.SpiralStaircase (spiralStaircase) where

import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (flipToOtherSide)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype SpiralStaircase = SpiralStaircase LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The [[Carcosa]] face of Bottomless Pit.
spiralStaircase :: LocationCard SpiralStaircase
spiralStaircase = locationWith SpiralStaircase Cards.spiralStaircase 3 (PerPlayer 1) (canBeFlippedL .~ True)

{- | "Forced - When Spiral Staircase flips: Move each investigator at Spiral
Staircase to a connecting location." Resolved as part of the flip, before the
staircase becomes Bottomless Pit underneath them.
-}
instance RunMessage SpiralStaircase where
  runMessage msg l@(SpiralStaircase attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      here <- select $ investigatorAt attrs.id
      for_ here \iid -> do
        connected <- select $ connectedTo (be attrs)
        chooseTargetM iid connected $ moveTo attrs iid
      flipToOtherSide attrs
      pure l
    _ -> SpiralStaircase <$> liftRunMessage msg attrs
