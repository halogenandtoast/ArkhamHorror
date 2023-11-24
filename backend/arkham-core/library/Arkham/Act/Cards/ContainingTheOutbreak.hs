module Arkham.Act.Cards.ContainingTheOutbreak
  ( ContainingTheOutbreak(..)
  , containingTheOutbreak
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype ContainingTheOutbreak = ContainingTheOutbreak ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

containingTheOutbreak :: ActCard ContainingTheOutbreak
containingTheOutbreak = act (3, A) ContainingTheOutbreak Cards.containingTheOutbreak Nothing

instance RunMessage ContainingTheOutbreak where
  runMessage msg (ContainingTheOutbreak attrs) = ContainingTheOutbreak <$> runMessage msg attrs
