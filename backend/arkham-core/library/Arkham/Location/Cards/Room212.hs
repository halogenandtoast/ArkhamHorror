module Arkham.Location.Cards.Room212 (
  room212,
  Room212 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Room212 = Room212 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

room212 :: LocationCard Room212
room212 = locationWith Room212 Cards.room212 0 (Static 0) (labelL .~ "room212")

instance HasAbilities Room212 where
  getAbilities (Room212 attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage Room212 where
  runMessage msg (Room212 attrs) =
    Room212 <$> runMessage msg attrs
