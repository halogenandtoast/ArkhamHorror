module Arkham.Act.Cards.EscapeTheTowerV1 (escapeTheTowerV1) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype EscapeTheTowerV1 = EscapeTheTowerV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapeTheTowerV1 :: ActCard EscapeTheTowerV1
escapeTheTowerV1 = act (2, A) EscapeTheTowerV1 Cards.escapeTheTowerV1 Nothing

-- TODO: abilities
instance HasAbilities EscapeTheTowerV1 where
  getAbilities _ = []

instance RunMessage EscapeTheTowerV1 where
  runMessage msg (EscapeTheTowerV1 attrs) = runQueueT $ EscapeTheTowerV1 <$> liftRunMessage msg attrs
