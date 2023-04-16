module Arkham.Act.Cards.InvestigatingTheWitchHouse
  ( InvestigatingTheWitchHouse(..)
  , investigatingTheWitchHouse
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype InvestigatingTheWitchHouse = InvestigatingTheWitchHouse ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

investigatingTheWitchHouse :: ActCard InvestigatingTheWitchHouse
investigatingTheWitchHouse =
  act (1, A) InvestigatingTheWitchHouse Cards.investigatingTheWitchHouse Nothing

instance RunMessage InvestigatingTheWitchHouse where
  runMessage msg (InvestigatingTheWitchHouse attrs) =
    InvestigatingTheWitchHouse <$> runMessage msg attrs
