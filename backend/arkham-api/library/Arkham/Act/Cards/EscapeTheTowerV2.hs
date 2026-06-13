module Arkham.Act.Cards.EscapeTheTowerV2 (escapeTheTowerV2) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype EscapeTheTowerV2 = EscapeTheTowerV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapeTheTowerV2 :: ActCard EscapeTheTowerV2
escapeTheTowerV2 = act (2, A) EscapeTheTowerV2 Cards.escapeTheTowerV2 Nothing

-- TODO: abilities
instance HasAbilities EscapeTheTowerV2 where
  getAbilities _ = []

instance RunMessage EscapeTheTowerV2 where
  runMessage msg (EscapeTheTowerV2 attrs) = runQueueT $ EscapeTheTowerV2 <$> liftRunMessage msg attrs
