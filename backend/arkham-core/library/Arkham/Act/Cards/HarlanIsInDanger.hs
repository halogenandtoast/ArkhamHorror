module Arkham.Act.Cards.HarlanIsInDanger
  ( HarlanIsInDanger(..)
  , harlanIsInDanger
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype HarlanIsInDanger = HarlanIsInDanger ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

harlanIsInDanger :: ActCard HarlanIsInDanger
harlanIsInDanger = act (1, A) HarlanIsInDanger Cards.harlanIsInDanger Nothing

instance RunMessage HarlanIsInDanger where
  runMessage msg (HarlanIsInDanger attrs) =
    HarlanIsInDanger <$> runMessage msg attrs
