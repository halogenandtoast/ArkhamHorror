module Arkham.Location.Cards.DancersMist
  ( dancersMist
  , DancersMist(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype DancersMist = DancersMist LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dancersMist :: LocationCard DancersMist
dancersMist = location DancersMist Cards.dancersMist 3 (Static 2)

instance HasAbilities DancersMist where
  getAbilities (DancersMist attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage DancersMist where
  runMessage msg (DancersMist attrs) =
    DancersMist <$> runMessage msg attrs
