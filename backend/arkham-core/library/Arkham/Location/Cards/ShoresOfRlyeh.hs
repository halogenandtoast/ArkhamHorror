module Arkham.Location.Cards.ShoresOfRlyeh
  ( shoresOfRlyeh
  , ShoresOfRlyeh(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ShoresOfRlyeh = ShoresOfRlyeh LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shoresOfRlyeh :: LocationCard ShoresOfRlyeh
shoresOfRlyeh = location ShoresOfRlyeh Cards.shoresOfRlyeh 1 (Static 2)

instance HasAbilities ShoresOfRlyeh where
  getAbilities (ShoresOfRlyeh attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage ShoresOfRlyeh where
  runMessage msg (ShoresOfRlyeh attrs) = ShoresOfRlyeh <$> runMessage msg attrs
