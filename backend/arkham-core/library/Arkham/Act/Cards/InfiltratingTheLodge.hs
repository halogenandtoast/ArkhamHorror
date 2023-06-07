module Arkham.Act.Cards.InfiltratingTheLodge
  ( InfiltratingTheLodge(..)
  , infiltratingTheLodge
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype InfiltratingTheLodge = InfiltratingTheLodge ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

infiltratingTheLodge :: ActCard InfiltratingTheLodge
infiltratingTheLodge = act (1, A) InfiltratingTheLodge Cards.infiltratingTheLodge Nothing

instance RunMessage InfiltratingTheLodge where
  runMessage msg (InfiltratingTheLodge attrs) = InfiltratingTheLodge <$> runMessage msg attrs
