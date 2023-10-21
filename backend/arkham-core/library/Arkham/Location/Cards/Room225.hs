module Arkham.Location.Cards.Room225 (
  room225,
  Room225 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Room225 = Room225 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

room225 :: LocationCard Room225
room225 = locationWith Room225 Cards.room225 3 (PerPlayer 1) (labelL .~ "room225")

instance HasAbilities Room225 where
  getAbilities (Room225 attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage Room225 where
  runMessage msg (Room225 attrs) =
    Room225 <$> runMessage msg attrs
