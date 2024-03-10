module Arkham.Location.Cards.TheDarkCrater (
  theDarkCrater,
  TheDarkCrater (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TheDarkCrater = TheDarkCrater LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDarkCrater :: LocationCard TheDarkCrater
theDarkCrater = location TheDarkCrater Cards.theDarkCrater 0 (Static 0)

instance HasAbilities TheDarkCrater where
  getAbilities (TheDarkCrater attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage TheDarkCrater where
  runMessage msg (TheDarkCrater attrs) =
    TheDarkCrater <$> runMessage msg attrs
