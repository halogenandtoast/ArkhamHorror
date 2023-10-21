module Arkham.Location.Cards.SuiteBalcony
  ( suiteBalcony
  , SuiteBalcony(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SuiteBalcony = SuiteBalcony LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suiteBalcony :: LocationCard SuiteBalcony
suiteBalcony = location SuiteBalcony Cards.suiteBalcony 2 (PerPlayer 1)

instance HasAbilities SuiteBalcony where
  getAbilities (SuiteBalcony attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage SuiteBalcony where
  runMessage msg (SuiteBalcony attrs) =
    SuiteBalcony <$> runMessage msg attrs
