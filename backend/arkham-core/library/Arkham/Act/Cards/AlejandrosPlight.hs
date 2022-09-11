module Arkham.Act.Cards.AlejandrosPlight
  ( AlejandrosPlight(..)
  , alejandrosPlight
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype AlejandrosPlight = AlejandrosPlight ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

alejandrosPlight :: ActCard AlejandrosPlight
alejandrosPlight = act (3, C) AlejandrosPlight Cards.alejandrosPlight Nothing

instance RunMessage AlejandrosPlight where
  runMessage msg (AlejandrosPlight attrs) =
    AlejandrosPlight <$> runMessage msg attrs
