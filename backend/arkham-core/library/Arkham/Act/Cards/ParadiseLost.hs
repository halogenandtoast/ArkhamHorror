module Arkham.Act.Cards.ParadiseLost
  ( ParadiseLost(..)
  , paradiseLost
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype ParadiseLost = ParadiseLost ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

paradiseLost :: ActCard ParadiseLost
paradiseLost = act (4, A) ParadiseLost Cards.paradiseLost Nothing

instance RunMessage ParadiseLost where
  runMessage msg (ParadiseLost attrs) = ParadiseLost <$> runMessage msg attrs
