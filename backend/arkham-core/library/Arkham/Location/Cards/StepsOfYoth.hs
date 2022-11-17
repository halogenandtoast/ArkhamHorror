module Arkham.Location.Cards.StepsOfYoth
  ( stepsOfYoth
  , StepsOfYoth(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype StepsOfYoth = StepsOfYoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stepsOfYoth :: LocationCard StepsOfYoth
stepsOfYoth = symbolLabel $ location StepsOfYoth Cards.stepsOfYoth 3 (Static 0)

instance HasAbilities StepsOfYoth where
  getAbilities (StepsOfYoth attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage StepsOfYoth where
  runMessage msg (StepsOfYoth attrs) =
    StepsOfYoth <$> runMessage msg attrs
