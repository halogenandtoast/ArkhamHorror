module Arkham.Location.Cards.DreamGatePointlessReality
  ( dreamGatePointlessReality
  , DreamGatePointlessReality(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype DreamGatePointlessReality = DreamGatePointlessReality LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamGatePointlessReality :: LocationCard DreamGatePointlessReality
dreamGatePointlessReality = location DreamGatePointlessReality Cards.dreamGatePointlessReality 0 (Static 0)

instance HasAbilities DreamGatePointlessReality where
  getAbilities (DreamGatePointlessReality attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage DreamGatePointlessReality where
  runMessage msg (DreamGatePointlessReality attrs) =
    DreamGatePointlessReality <$> runMessage msg attrs
