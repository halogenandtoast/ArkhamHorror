module Arkham.Agenda.Cards.ArkhamAlive (arkhamAlive) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query
import Arkham.Matcher.Card (cardIs)
import Arkham.Scenarios.SmokeAndMirrors.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype ArkhamAlive = ArkhamAlive AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamAlive :: AgendaCard ArkhamAlive
arkhamAlive = agenda (1, A) ArkhamAlive Cards.arkhamAlive (Static 10)

instance HasAbilities ArkhamAlive where
  getAbilities (ArkhamAlive a) =
    [scenarioI18n $ withI18nTooltip "arkhamAlive.resign " $ mkAbility a 1 resignAction_ | onSide A a]

instance RunMessage ArkhamAlive where
  runMessage msg a@(ArkhamAlive attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      investigators <- allInvestigators
      cards <- getSetAsideCardsMatching (cardIs Treacheries.markOfElokoss)
      for_ (zip investigators cards) \(iid, card) -> do
        createWeaknessInThreatArea card iid
        addCampaignCardToDeck iid DoNotShuffleIn card
      advanceAgendaDeck attrs
      pure a
    _ -> ArkhamAlive <$> liftRunMessage msg attrs
