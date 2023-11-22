module Arkham.Location.Cards.EnchantedWoodsMysticalForest (
  enchantedWoodsMysticalForest,
  EnchantedWoodsMysticalForest (..),
)
where

import Arkham.Card
import Arkham.Discard
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype EnchantedWoodsMysticalForest = EnchantedWoodsMysticalForest LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedWoodsMysticalForest :: LocationCard EnchantedWoodsMysticalForest
enchantedWoodsMysticalForest = location EnchantedWoodsMysticalForest Cards.enchantedWoodsMysticalForest 4 (PerPlayer 1)

instance HasModifiersFor EnchantedWoodsMysticalForest where
  getModifiersFor (InvestigatorTarget iid) (EnchantedWoodsMysticalForest attrs) = do
    here <- iid `isAt` attrs
    if here
      then do
        clues <- field LocationClues (toId attrs)
        discardableCards <- fieldMap InvestigatorHand (filter (`cardMatch` DiscardableCard)) iid
        pure $ toModifiers attrs [CannotMove | clues > 0, length discardableCards < clues]
      else pure []
  getModifiersFor _ _ = pure []

instance HasAbilities EnchantedWoodsMysticalForest where
  getAbilities (EnchantedWoodsMysticalForest attrs) =
    getAbilities attrs

instance RunMessage EnchantedWoodsMysticalForest where
  runMessage msg l@(EnchantedWoodsMysticalForest attrs) = case msg of
    Will (MoveFrom _ iid fromLocationId) | fromLocationId == toId attrs -> do
      here <- iid `isAt` attrs
      clues <- field LocationClues (toId attrs)
      pushWhen here
        $ discardFromHand iid (toSource attrs) DiscardChoose clues
      pure l
    _ -> EnchantedWoodsMysticalForest <$> runMessage msg attrs
