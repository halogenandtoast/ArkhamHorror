module Arkham.Act.Cards.ALostLegacy (aLostLegacy) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ALostLegacy = ALostLegacy ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

aLostLegacy :: ActCard ALostLegacy
aLostLegacy = act (1, A) ALostLegacy Cards.aLostLegacy Nothing

instance RunMessage ALostLegacy where
  runMessage msg a@(ALostLegacy attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> ALostLegacy <$> liftRunMessage msg attrs
