module Arkham.Types.Agenda.Cards.TheFirstNight (
  TheFirstNight,
  theFirstNight,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Scenarios.APhantomOfTruth.Helpers
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Modifier

newtype TheFirstNight = TheFirstNight AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFirstNight :: AgendaCard TheFirstNight
theFirstNight = agenda (1, A) TheFirstNight Cards.theFirstNight (Static 12)

instance HasRecord env () => HasModifiersFor env TheFirstNight where
  getModifiersFor _ target (TheFirstNight a) | not (isTarget a target) = do
    conviction <- getRecordCount Conviction
    doubt <- getRecordCount Doubt
    pure $ toModifiers a $ [DoomSubtracts | conviction > doubt]
  getModifiersFor _ _ _ = pure []

instance AgendaRunner env => RunMessage env TheFirstNight where
  runMessage msg a@(TheFirstNight attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      msgs <- disengageEachEnemyAndMoveToConnectingLocation
      pushAll $ msgs <> [NextAdvanceAgendaStep (toId attrs) 2]
      pure a
    NextAdvanceAgendaStep aid 2 | aid == toId attrs && onSide B attrs -> do
      organistMsg <- moveOrganistAwayFromNearestInvestigator
      a <$ pushAll (organistMsg : [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)])
    _ -> TheFirstNight <$> runMessage msg attrs
