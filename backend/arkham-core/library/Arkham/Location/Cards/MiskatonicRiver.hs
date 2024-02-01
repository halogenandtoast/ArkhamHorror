module Arkham.Location.Cards.MiskatonicRiver (
  miskatonicRiver,
  MiskatonicRiver (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MiskatonicRiver = MiskatonicRiver LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

miskatonicRiver :: LocationCard MiskatonicRiver
miskatonicRiver = location MiskatonicRiver Cards.miskatonicRiver 5 (Static 0)

instance HasAbilities MiskatonicRiver where
  getAbilities (MiskatonicRiver attrs) =
    withRevealedAbilities
      attrs
      [ withTooltip
          "You take your rowboat back to the shore of the Miskatonic River, leaving the mysterious island behind."
          (locationResignAction attrs)
      ]

instance RunMessage MiskatonicRiver where
  runMessage msg (MiskatonicRiver attrs) =
    MiskatonicRiver <$> runMessage msg attrs
