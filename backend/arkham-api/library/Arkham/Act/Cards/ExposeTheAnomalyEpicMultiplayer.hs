module Arkham.Act.Cards.ExposeTheAnomalyEpicMultiplayer (exposeTheAnomalyEpicMultiplayer) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ExposeTheAnomalyEpicMultiplayer = ExposeTheAnomalyEpicMultiplayer ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposeTheAnomalyEpicMultiplayer :: ActCard ExposeTheAnomalyEpicMultiplayer
exposeTheAnomalyEpicMultiplayer = act (1, A) ExposeTheAnomalyEpicMultiplayer Cards.exposeTheAnomalyEpicMultiplayer Nothing

instance RunMessage ExposeTheAnomalyEpicMultiplayer where
  runMessage msg (ExposeTheAnomalyEpicMultiplayer attrs) = ExposeTheAnomalyEpicMultiplayer <$> runMessage msg attrs
