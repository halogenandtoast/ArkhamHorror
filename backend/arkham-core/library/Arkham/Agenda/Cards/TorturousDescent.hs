module Arkham.Agenda.Cards.TorturousDescent
  ( TorturousDescent(..)
  , torturousDescent
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Agenda.Types
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Deck

newtype TorturousDescent = TorturousDescent AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

torturousDescent :: AgendaCard TorturousDescent
torturousDescent =
  agenda (2, A) TorturousDescent Cards.torturousDescent (Static 7)

instance RunMessage TorturousDescent where
  runMessage msg a@(TorturousDescent attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      spawnConstanceDumaine <-
        notElem (Recorded $ toCardCode Enemies.constanceDumaine)
          <$> getRecordSet VIPsSlain

      spawnConstanceDumaineMessages <- if spawnConstanceDumaine
        then do
          card <- genCard Enemies.constanceDumaine
          pure [CreateEnemyAtLocationMatching card (LocationWithTitle "Garden")]
        else pure []
      a <$ pushAll
        ([ DrawRandomFromScenarioDeck
             leadInvestigatorId
             MonstersDeck
             (toTarget attrs)
             1
         ]
        <> spawnConstanceDumaineMessages
        <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
        )
    DrewFromScenarioDeck _ _ target cards | isTarget attrs target ->
      a <$ push
        (ShuffleIntoEncounterDeck $ mapMaybe (preview _EncounterCard) cards)
    _ -> TorturousDescent <$> runMessage msg attrs
