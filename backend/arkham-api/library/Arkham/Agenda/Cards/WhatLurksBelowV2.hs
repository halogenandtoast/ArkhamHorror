module Arkham.Agenda.Cards.WhatLurksBelowV2 (WhatLurksBelowV2 (..), whatLurksBelowV2) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.TheLairOfDagon.Helpers

newtype WhatLurksBelowV2 = WhatLurksBelowV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatLurksBelowV2 :: AgendaCard WhatLurksBelowV2
whatLurksBelowV2 = agenda (2, A) WhatLurksBelowV2 Cards.whatLurksBelowV2 (Static 6)

instance HasAbilities WhatLurksBelowV2 where
  getAbilities (WhatLurksBelowV2 a) = [needsAir a 1]

instance RunMessage WhatLurksBelowV2 where
  runMessage msg a@(WhatLurksBelowV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      tokens <- take 4 <$> select (ChaosTokenFaceIs #curse)
      push $ ReturnChaosTokensToPool tokens
      lead <- getLead
      chooseFromM lead UneliminatedInvestigator \iid -> do
        thomasDawson <- createAsset =<< getSetAsideCard Assets.thomasDawsonSoldierInANewWar
        gameModifier attrs thomasDawson (DoNotTakeUpSlot #ally)
        takeControlOfAsset iid thomasDawson
        ks <- scenarioField ScenarioKeys
        unless (null ks) do
          chooseOneM iid $ scenarioI18n $ scope "whatLurksBelow" do
            labeled' "doNotTakeKey" nothing
            for_ ks \k -> keyVar "key" (keyName k) $ labeled' "takeKey" (placeKey iid k)
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> WhatLurksBelowV2 <$> liftRunMessage msg attrs
