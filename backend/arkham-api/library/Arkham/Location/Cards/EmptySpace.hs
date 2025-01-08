module Arkham.Location.Cards.EmptySpace (emptySpace, EmptySpace (..)) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype EmptySpace = EmptySpace LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emptySpace :: LocationCard EmptySpace
emptySpace =
  locationWith EmptySpace Cards.emptySpace 0 (Static 0)
    $ connectsToL
    .~ adjacentLocations

instance HasModifiersFor EmptySpace where
  getModifiersFor (EmptySpace a) = modifySelf a [IsEmptySpace]

instance HasAbilities EmptySpace where
  getAbilities _ = []

instance RunMessage EmptySpace where
  runMessage msg (EmptySpace attrs) = EmptySpace <$> runMessage msg attrs
