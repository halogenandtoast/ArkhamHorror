module Arkham.Location.Cards.DarkSpires (
  darkSpires,
  DarkSpires (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype DarkSpires = DarkSpires LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

darkSpires :: LocationCard DarkSpires
darkSpires =
  locationWith
    DarkSpires
    Cards.darkSpires
    3
    (PerPlayer 2)
    ((canBeFlippedL .~ True) . (revealedL .~ True))

instance RunMessage DarkSpires where
  runMessage msg (DarkSpires attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theFall
      pure . DarkSpires $ attrs & canBeFlippedL .~ False
    _ -> DarkSpires <$> runMessage msg attrs
