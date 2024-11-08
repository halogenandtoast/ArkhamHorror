module Arkham.Agenda.Cards.WhatLurksBelowV1 (WhatLurksBelowV1 (..), whatLurksBelowV1) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Helpers.ChaosBag

newtype WhatLurksBelowV1 = WhatLurksBelowV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatLurksBelowV1 :: AgendaCard WhatLurksBelowV1
whatLurksBelowV1 = agenda (2, A) WhatLurksBelowV1 Cards.whatLurksBelowV1 (Static 6)

instance HasAbilities WhatLurksBelowV1 where
  getAbilities (WhatLurksBelowV1 a) = [needsAir a 1]

instance RunMessage WhatLurksBelowV1 where
  runMessage msg a@(WhatLurksBelowV1 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      n <- min 4 <$> getRemainingCurseTokens
      replicateM_ n $ addChaosToken #curse
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> WhatLurksBelowV1 <$> liftRunMessage msg attrs
