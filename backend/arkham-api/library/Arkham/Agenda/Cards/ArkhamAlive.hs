module Arkham.Agenda.Cards.ArkhamAlive (ArkhamAlive (..), arkhamAlive) where

-- Constructor is only exported for testing purposes

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.BrethrenOfAsh.Key
import Arkham.Helpers.Query
import Arkham.Investigator.Types (Field (InvestigatorCardCode))
import Arkham.Matcher.Card (cardIs)
import Arkham.Message.Lifted.Log (recordSetInsert)
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Treacheries

newtype ArkhamAlive = ArkhamAlive AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamAlive :: AgendaCard ArkhamAlive
arkhamAlive = agenda (1, A) ArkhamAlive Cards.arkhamAlive (Static 10)

instance HasAbilities ArkhamAlive where
  getAbilities (ArkhamAlive a) =
    [ withTooltip
        "_Resign_. You return to compare notes with Dr. Armitage on your friend's whereabouts."
        $ restricted a 1 NoRestriction
        $ ActionAbility #resign Nothing
        $ ActionCost 1
    | onSide A a
    ]

instance RunMessage ArkhamAlive where
  runMessage msg a@(ArkhamAlive attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Resign iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      investigators <- allInvestigators
      cards <- getSetAsideCardsMatching (cardIs Treacheries.markOfElokoss)
      codes <- traverse (field InvestigatorCardCode) investigators
      recordSetInsert InvestigatorsWereMarkedByElokoss codes
      for_ (zip investigators cards) \(iid, card) -> do
        createWeaknessInThreatArea card iid
      advanceAgendaDeck attrs
      pure a
    _ -> ArkhamAlive <$> liftRunMessage msg attrs
