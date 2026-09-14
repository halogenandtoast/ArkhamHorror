module Arkham.Agenda.Cards.TheScarletKeys.RiddlesAndRain.TheConnection (theConnection) where

import Arkham.Agenda.CardDefs.TheScarletKeys.RiddlesAndRain qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Enemy.CardDefs.TheScarletKeys.CrimsonConspiracy qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheScarletKeys.RiddlesAndRain.Helpers

newtype TheConnection = TheConnection AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theConnection :: AgendaCard TheConnection
theConnection = agenda (3, A) TheConnection Cards.theConnection (Static 9)

instance RunMessage TheConnection where
  runMessage msg a@(TheConnection attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      leadChooseOneM $ scenarioI18n do
        labeled "theConnection.coterieAgent" do
          findAndDrawEncounterCard lead
            $ mapOneOf cardIs [Enemies.coterieAgentA, Enemies.coterieAgentB, Enemies.coterieAgentC]
          shuffleDeck Deck.EncounterDeck
        labeled "theConnection.damageAndHorror" do
          eachInvestigator \iid -> assignDamageAndHorror iid attrs 1 1
      advanceAgendaDeck attrs
      pure a
    _ -> TheConnection <$> liftRunMessage msg attrs
