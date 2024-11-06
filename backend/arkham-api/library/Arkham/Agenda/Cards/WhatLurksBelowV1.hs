module Arkham.Agenda.Cards.WhatLurksBelowV1 (
  WhatLurksBelowV1 (..),
  whatLurksBelowV1,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype WhatLurksBelowV1 = WhatLurksBelowV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatLurksBelowV1 :: AgendaCard WhatLurksBelowV1
whatLurksBelowV1 = agenda (2, A) WhatLurksBelowV1 Cards.whatLurksBelowV1 (Static 6)

instance RunMessage WhatLurksBelowV1 where
  runMessage msg a@(WhatLurksBelowV1 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> WhatLurksBelowV1 <$> liftRunMessage msg attrs
