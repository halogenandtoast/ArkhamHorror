module Arkham.Location.Cards.PlainOfTheGhouls (
  plainOfTheGhouls,
  PlainOfTheGhouls (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype PlainOfTheGhouls = PlainOfTheGhouls LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plainOfTheGhouls :: LocationCard PlainOfTheGhouls
plainOfTheGhouls = location PlainOfTheGhouls Cards.plainOfTheGhouls 4 (PerPlayer 1)

instance HasAbilities PlainOfTheGhouls where
  getAbilities (PlainOfTheGhouls attrs) =
    extendRevealed attrs []

instance RunMessage PlainOfTheGhouls where
  runMessage msg (PlainOfTheGhouls attrs) =
    PlainOfTheGhouls <$> runMessage msg attrs
