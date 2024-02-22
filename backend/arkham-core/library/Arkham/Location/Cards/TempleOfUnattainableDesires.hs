module Arkham.Location.Cards.TempleOfUnattainableDesires (templeOfUnattainableDesires, TempleOfUnattainableDesires (..)) where

import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Story

newtype TempleOfUnattainableDesires = TempleOfUnattainableDesires LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeOfUnattainableDesires :: LocationCard TempleOfUnattainableDesires
templeOfUnattainableDesires = location TempleOfUnattainableDesires Cards.templeOfUnattainableDesires 3 (PerPlayer 1)

instance HasModifiersFor TempleOfUnattainableDesires where
  getModifiersFor target (TempleOfUnattainableDesires attrs) | isTarget attrs target = do
    beseechedTheKing <- remembered BeseechedTheKing
    pure $ toModifiers attrs [Blocked | not (locationRevealed attrs) && not beseechedTheKing]
  getModifiersFor _ _ = pure []

instance HasAbilities TempleOfUnattainableDesires where
  getAbilities (TempleOfUnattainableDesires attrs) = veiled attrs []

instance RunMessage TempleOfUnattainableDesires where
  runMessage msg (TempleOfUnattainableDesires attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theCityInside
      pure . TempleOfUnattainableDesires $ attrs & canBeFlippedL .~ False
    _ -> TempleOfUnattainableDesires <$> runMessage msg attrs
