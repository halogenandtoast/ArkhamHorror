module Arkham.Act.Cards.FatedSouls (
  FatedSouls (..),
  fatedSouls,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype FatedSouls = FatedSouls ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor FatedSouls where
  getModifiersFor (InvestigatorTarget _) (FatedSouls attrs) = do
    pure $ toModifiers attrs [CannotMove, CannotBeMoved]
  getModifiersFor _ _ = pure []

fatedSouls :: ActCard FatedSouls
fatedSouls = act (2, A) FatedSouls Cards.fatedSouls (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance RunMessage FatedSouls where
  runMessage msg (FatedSouls attrs) = FatedSouls <$> runMessage msg attrs
