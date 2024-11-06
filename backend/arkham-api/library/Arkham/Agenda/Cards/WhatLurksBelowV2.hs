module Arkham.Agenda.Cards.WhatLurksBelowV2 (
  WhatLurksBelowV2 (..),
  whatLurksBelowV2,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype WhatLurksBelowV2 = WhatLurksBelowV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatLurksBelowV2 :: AgendaCard WhatLurksBelowV2
whatLurksBelowV2 = agenda (2, A) WhatLurksBelowV2 Cards.whatLurksBelowV2 (Static 6)

instance RunMessage WhatLurksBelowV2 where
  runMessage msg a@(WhatLurksBelowV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> WhatLurksBelowV2 <$> liftRunMessage msg attrs
