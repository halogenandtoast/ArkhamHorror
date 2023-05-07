module Arkham.Location.Cards.HauntedFields (
  hauntedFields,
  HauntedFields (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner

newtype HauntedFields = HauntedFields LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hauntedFields :: LocationCard HauntedFields
hauntedFields = location HauntedFields Cards.hauntedFields 3 (PerPlayer 2)

instance HasAbilities HauntedFields where
  getAbilities (HauntedFields attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage HauntedFields where
  runMessage msg l@(HauntedFields attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.hauntedFieldsSpectral
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> HauntedFields <$> runMessage msg attrs
