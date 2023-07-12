module Arkham.Act.Cards.FatedSouls
  ( FatedSouls(..)
  , fatedSouls
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher

newtype FatedSouls = FatedSouls ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

fatedSouls :: ActCard FatedSouls
fatedSouls = act (2, A) FatedSouls Cards.fatedSouls (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance RunMessage FatedSouls where
  runMessage msg (FatedSouls attrs) = FatedSouls <$> runMessage msg attrs
