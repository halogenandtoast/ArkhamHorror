module Arkham.Location.Cards.EnchantedWoodsMysticalForest (
  enchantedWoodsMysticalForest,
  EnchantedWoodsMysticalForest (..),
)
where

import Arkham.Card
import Arkham.Discard
import Arkham.GameValue
import Arkham.Helpers.Location (isAt)
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype EnchantedWoodsMysticalForest = EnchantedWoodsMysticalForest LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

enchantedWoodsMysticalForest :: LocationCard EnchantedWoodsMysticalForest
enchantedWoodsMysticalForest = location EnchantedWoodsMysticalForest Cards.enchantedWoodsMysticalForest 4 (PerPlayer 1)

instance HasModifiersFor EnchantedWoodsMysticalForest where
  getModifiersFor (EnchantedWoodsMysticalForest a) = modifySelectMaybe a (investigatorAt a) \iid -> do
    clues <- lift $ field LocationClues a.id
    discardableCards <- lift $ fieldMap InvestigatorHand (filter (`cardMatch` DiscardableCard)) iid
    guard $ clues > 0 && length discardableCards < clues
    pure [CannotMove]

instance RunMessage EnchantedWoodsMysticalForest where
  runMessage msg l@(EnchantedWoodsMysticalForest attrs) = runQueueT $ case msg of
    Will (MoveFrom _ iid fromLocationId) | fromLocationId == attrs.id -> do
      here <- iid `isAt` attrs
      when here do
        clues <- field LocationClues attrs.id
        discardFromHand iid attrs DiscardChoose clues
      pure l
    _ -> EnchantedWoodsMysticalForest <$> liftRunMessage msg attrs
