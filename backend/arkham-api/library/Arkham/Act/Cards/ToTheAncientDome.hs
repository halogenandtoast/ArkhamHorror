module Arkham.Act.Cards.ToTheAncientDome (toTheAncientDome) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ToTheAncientDome = ToTheAncientDome ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toTheAncientDome :: ActCard ToTheAncientDome
toTheAncientDome = act (2, A) ToTheAncientDome Cards.toTheAncientDome Nothing

-- TODO: abilities
instance HasAbilities ToTheAncientDome where
  getAbilities _ = []

instance RunMessage ToTheAncientDome where
  runMessage msg (ToTheAncientDome attrs) = runQueueT $ ToTheAncientDome <$> liftRunMessage msg attrs
