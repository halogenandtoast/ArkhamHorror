module Arkham.Location.Cards.HauntedFields (
  hauntedFields,
  HauntedFields (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait (Trait (Spectral))

newtype HauntedFields = HauntedFields LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

hauntedFields :: LocationCard HauntedFields
hauntedFields = location HauntedFields Cards.hauntedFields 3 (PerPlayer 2)

instance HasModifiersFor HauntedFields where
  getModifiersFor (EnemyTarget eid) (HauntedFields attrs) = do
    affected <- eid <=~> (enemyAt (toId attrs) <> EnemyWithTrait Spectral)
    pure $ toModifiers attrs [HorrorDealt 1 | affected]
  getModifiersFor _ _ = pure []

instance RunMessage HauntedFields where
  runMessage msg l@(HauntedFields attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.hauntedFieldsSpectral
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> HauntedFields <$> runMessage msg attrs
