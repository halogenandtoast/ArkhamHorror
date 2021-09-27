module Arkham.Types.Location.Cards.Study where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (study)
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype Study = Study LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

study :: LocationCard Study
study = location Study Cards.study 2 (PerPlayer 2) Circle []

instance LocationRunner env => RunMessage env Study where
  runMessage msg (Study attrs) = Study <$> runMessage msg attrs
