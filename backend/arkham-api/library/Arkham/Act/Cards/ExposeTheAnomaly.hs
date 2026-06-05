module Arkham.Act.Cards.ExposeTheAnomaly (exposeTheAnomaly) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ExposeTheAnomaly = ExposeTheAnomaly ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposeTheAnomaly :: ActCard ExposeTheAnomaly
exposeTheAnomaly = act (1, A) ExposeTheAnomaly Cards.exposeTheAnomaly Nothing

instance RunMessage ExposeTheAnomaly where
  runMessage msg (ExposeTheAnomaly attrs) = ExposeTheAnomaly <$> runMessage msg attrs
