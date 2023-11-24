module Arkham.Location.Cards.ExperimentalTherapiesWard
  ( experimentalTherapiesWard
  , ExperimentalTherapiesWard(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ExperimentalTherapiesWard = ExperimentalTherapiesWard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

experimentalTherapiesWard :: LocationCard ExperimentalTherapiesWard
experimentalTherapiesWard = location ExperimentalTherapiesWard Cards.experimentalTherapiesWard 4 (PerPlayer 2)

instance HasAbilities ExperimentalTherapiesWard where
  getAbilities (ExperimentalTherapiesWard attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage ExperimentalTherapiesWard where
  runMessage msg (ExperimentalTherapiesWard attrs) =
    ExperimentalTherapiesWard <$> runMessage msg attrs
