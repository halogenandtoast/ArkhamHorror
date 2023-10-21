module Arkham.Location.Cards.Room245 (
  room245,
  Room245 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Room245 = Room245 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

room245 :: LocationCard Room245
room245 = locationWith Room245 Cards.room245 0 (Static 0) (labelL .~ "room245")

instance HasAbilities Room245 where
  getAbilities (Room245 attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage Room245 where
  runMessage msg (Room245 attrs) =
    Room245 <$> runMessage msg attrs
