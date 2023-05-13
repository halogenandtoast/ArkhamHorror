module Arkham.Location.Cards.HangmansBrook (
  hangmansBrook,
  HangmansBrook (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner

newtype HangmansBrook = HangmansBrook LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangmansBrook :: LocationCard HangmansBrook
hangmansBrook = location HangmansBrook Cards.hangmansBrook 4 (PerPlayer 1)

instance HasAbilities HangmansBrook where
  getAbilities (HangmansBrook a) =
    withBaseAbilities
      a
      [ withTooltip
        "\"Who's bright idea was this, anyway?\""
        (locationResignAction a)
      | locationRevealed a
      ]

instance RunMessage HangmansBrook where
  runMessage msg l@(HangmansBrook attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.hangmansBrookSpectral
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> HangmansBrook <$> runMessage msg attrs
