module Arkham.Location.Cards.NightOfTheZealot.TheGathering.Study (study) where

import Arkham.GameValue
import Arkham.Location.CardDefs.NightOfTheZealot.TheGathering qualified as Cards
import Arkham.Location.Import.Lifted

newtype Study = Study LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

study :: LocationCard Study
study = location Study Cards.study 2 (PerPlayer 2)

instance RunMessage Study where
  runMessage msg (Study attrs) = Study <$> runMessage msg attrs
