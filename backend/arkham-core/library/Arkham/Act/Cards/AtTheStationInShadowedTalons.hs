module Arkham.Act.Cards.AtTheStationInShadowedTalons
  ( AtTheStationInShadowedTalons(..)
  , atTheStationInShadowedTalons
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype AtTheStationInShadowedTalons = AtTheStationInShadowedTalons ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

atTheStationInShadowedTalons :: ActCard AtTheStationInShadowedTalons
atTheStationInShadowedTalons = act
  (2, C)
  AtTheStationInShadowedTalons
  Cards.atTheStationInShadowedTalons
  Nothing

instance RunMessage AtTheStationInShadowedTalons where
  runMessage msg (AtTheStationInShadowedTalons attrs) =
    AtTheStationInShadowedTalons <$> runMessage msg attrs
