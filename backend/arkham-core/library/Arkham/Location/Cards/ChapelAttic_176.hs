module Arkham.Location.Cards.ChapelAttic_176 (
  chapelAttic_176,
  ChapelAttic_176 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Projection

newtype ChapelAttic_176 = ChapelAttic_176 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

chapelAttic_176 :: LocationCard ChapelAttic_176
chapelAttic_176 = location ChapelAttic_176 Cards.chapelAttic_176 8 (Static 0)

instance HasModifiersFor ChapelAttic_176 where
  getModifiersFor (InvestigatorTarget iid) (ChapelAttic_176 a) = do
    investigating <- isInvestigating iid (toId a)
    cardCount <- fieldMap InvestigatorHand length iid
    pure $ toModifiers a [AnySkillValue cardCount | investigating]
  getModifiersFor _ _ = pure []

instance RunMessage ChapelAttic_176 where
  runMessage msg l@(ChapelAttic_176 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.chapelAtticSpectral_176
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> ChapelAttic_176 <$> runMessage msg attrs
