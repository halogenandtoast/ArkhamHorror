module Arkham.Location.Cards.SeventySteps
  ( seventySteps
  , SeventySteps(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SeventySteps = SeventySteps LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seventySteps :: LocationCard SeventySteps
seventySteps = location SeventySteps Cards.seventySteps 1 (PerPlayer 1)

instance HasAbilities SeventySteps where
  getAbilities (SeventySteps attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage SeventySteps where
  runMessage msg (SeventySteps attrs) =
    SeventySteps <$> runMessage msg attrs
