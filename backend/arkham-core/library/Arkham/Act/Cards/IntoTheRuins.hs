module Arkham.Act.Cards.IntoTheRuins
  ( IntoTheRuins(..)
  , intoTheRuins
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype IntoTheRuins = IntoTheRuins ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

intoTheRuins :: ActCard IntoTheRuins
intoTheRuins = act (1, A) IntoTheRuins Cards.intoTheRuins Nothing

instance RunMessage IntoTheRuins where
  runMessage msg (IntoTheRuins attrs) = IntoTheRuins <$> runMessage msg attrs
