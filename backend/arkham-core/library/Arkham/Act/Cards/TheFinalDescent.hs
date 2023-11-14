module Arkham.Act.Cards.TheFinalDescent (
  TheFinalDescent (..),
  theFinalDescent,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheFinalDescent = TheFinalDescent ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theFinalDescent :: ActCard TheFinalDescent
theFinalDescent = act (3, A) TheFinalDescent Cards.theFinalDescent Nothing

instance RunMessage TheFinalDescent where
  runMessage msg (TheFinalDescent attrs) = TheFinalDescent <$> runMessage msg attrs
