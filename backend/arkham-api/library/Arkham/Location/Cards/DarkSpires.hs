module Arkham.Location.Cards.DarkSpires (darkSpires) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype DarkSpires = DarkSpires LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

darkSpires :: LocationCard DarkSpires
darkSpires =
  locationWith DarkSpires Cards.darkSpires 3 (PerPlayer 2)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance RunMessage DarkSpires where
  runMessage msg (DarkSpires attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theFall
      pure . DarkSpires $ attrs & canBeFlippedL .~ False
    _ -> DarkSpires <$> liftRunMessage msg attrs
