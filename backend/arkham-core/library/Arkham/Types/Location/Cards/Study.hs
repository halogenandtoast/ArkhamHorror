module Arkham.Types.Location.Cards.Study where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (study)
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype Study = Study LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities env)

study :: LocationCard Study
study = location Study Cards.study 2 (PerPlayer 2) Circle []

instance LocationRunner env => RunMessage env Study where
  runMessage msg (Study attrs) = Study <$> runMessage msg attrs
