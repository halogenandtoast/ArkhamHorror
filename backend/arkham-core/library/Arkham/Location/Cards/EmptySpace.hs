module Arkham.Location.Cards.EmptySpace (
  emptySpace,
  EmptySpace (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype EmptySpace = EmptySpace LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emptySpace :: LocationCard EmptySpace
emptySpace =
  locationWith
    EmptySpace
    Cards.emptySpace
    0
    (Static 0)
    (connectsToL .~ adjacentLocations)

instance HasModifiersFor EmptySpace where
  getModifiersFor target (EmptySpace a) | isTarget a target = do
    pure $ toModifiers a [IsEmptySpace]
  getModifiersFor _ _ = pure []

instance HasAbilities EmptySpace where
  getAbilities _ = []

instance RunMessage EmptySpace where
  runMessage msg (EmptySpace attrs) =
    EmptySpace <$> runMessage msg attrs
