module Arkham.Location.Cards.Study where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (study)
import Arkham.Location.Runner

newtype Study = Study LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

study :: LocationCard Study
study = location Study Cards.study 2 (PerPlayer 2)

instance RunMessage Study where
  runMessage msg (Study attrs) = Study <$> runMessage msg attrs
