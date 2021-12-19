module Arkham.Types.Agenda.Cards.TheSecondNight
  ( TheSecondNight
  , theSecondNight
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Scenarios.APhantomOfTruth.Helpers
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message

newtype TheSecondNight = TheSecondNight AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecondNight :: AgendaCard TheSecondNight
theSecondNight = agenda (2, A) TheSecondNight Cards.theSecondNight (Static 5)

instance AgendaRunner env => RunMessage env TheSecondNight where
  runMessage msg a@(TheSecondNight attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      msgs <- disengageEachEnemyAndMoveToConnectingLocation
      pushAll $ msgs <> [NextAdvanceAgendaStep (toId attrs) 2]
      pure a
    NextAdvanceAgendaStep aid 2 | aid == toId attrs && onSide B attrs -> do
      organistMsg <- moveOrganistAwayFromNearestInvestigator
      spawnJordanPerry <-
        notElem (Recorded $ toCardCode Enemies.jordanPerry)
          <$> getRecordSet VIPsSlain
      spawnJordanPerryMessages <- if spawnJordanPerry
        then do
          card <- genCard Enemies.jordanPerry
          pure
            [ CreateEnemyAtLocationMatching
                card
                (LocationWithTitle "Montparnasse")
            ]
        else pure []
      a <$ pushAll (organistMsg : spawnJordanPerryMessages <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)])
    _ -> TheSecondNight <$> runMessage msg attrs
