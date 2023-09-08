module Arkham.Act.Cards.TheCosmosBeckons
  ( TheCosmosBeckons(..)
  , theCosmosBeckons
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheCosmosBeckons = TheCosmosBeckons ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theCosmosBeckons :: ActCard TheCosmosBeckons
theCosmosBeckons = act (1, A) TheCosmosBeckons Cards.theCosmosBeckons Nothing

instance RunMessage TheCosmosBeckons where
  runMessage msg (TheCosmosBeckons attrs) = TheCosmosBeckons <$> runMessage msg attrs
