module Arkham.Act.Cards.InAzathothsDomain
  ( InAzathothsDomain(..)
  , inAzathothsDomain
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype InAzathothsDomain = InAzathothsDomain ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

inAzathothsDomain :: ActCard InAzathothsDomain
inAzathothsDomain = act (2, A) InAzathothsDomain Cards.inAzathothsDomain Nothing

instance RunMessage InAzathothsDomain where
  runMessage msg (InAzathothsDomain attrs) = InAzathothsDomain <$> runMessage msg attrs
