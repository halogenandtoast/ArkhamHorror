module Arkham.Types.Agenda.Cards.TorturousDescent
  ( TorturousDescent
  , torturousDescent
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Scenario.Deck

newtype TorturousDescent = TorturousDescent AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

torturousDescent :: AgendaCard TorturousDescent
torturousDescent =
  agenda (2, A) TorturousDescent Cards.torturousDescent (Static 7)

instance AgendaRunner env => RunMessage env TorturousDescent where
  runMessage msg a@(TorturousDescent attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      spawnConstanceDumaine <-
        elem (Recorded $ toCardCode Enemies.constanceDumaine)
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
