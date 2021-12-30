module Arkham.Agenda.Cards.TheSecondNight
  ( TheSecondNight
  , theSecondNight
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Scenarios.APhantomOfTruth.Helpers
import Arkham.Agenda.Attrs
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier

newtype TheSecondNight = TheSecondNight AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecondNight :: AgendaCard TheSecondNight
theSecondNight = agenda (2, A) TheSecondNight Cards.theSecondNight (Static 5)

instance HasRecord env () => HasModifiersFor env TheSecondNight where
  getModifiersFor _ target (TheSecondNight a) | not (isTarget a target) = do
    conviction <- getRecordCount Conviction
    doubt <- getRecordCount Doubt
    pure $ toModifiers a $ [DoomSubtracts | conviction > doubt]
  getModifiersFor _ _ _ = pure []

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
