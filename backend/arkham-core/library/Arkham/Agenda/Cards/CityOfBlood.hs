module Arkham.Agenda.Cards.CityOfBlood
  ( CityOfBlood(..)
  , cityOfBlood
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype CityOfBlood = CityOfBlood AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfBlood :: AgendaCard CityOfBlood
cityOfBlood = agenda (4, A) CityOfBlood Cards.cityOfBlood (Static 4)

instance RunMessage CityOfBlood where
  runMessage msg a@(CityOfBlood attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      mHarbinger <- selectOne $ SetAsideCardMatch $ cardIs
        Enemies.harbingerOfValusia
      let
        harbingerMsgs =
          [ CreateEnemyWithPlacement harbinger Pursuit
          | harbinger <- toList mHarbinger
          ]
      pushAll
        $ harbingerMsgs
        <> [ NextAdvanceAgendaStep (toId attrs) 1
           , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
           ]
      pure a
    NextAdvanceAgendaStep aid 1 | aid == toId attrs -> do
      enemyMsgs <- getPlacePursuitEnemyMessages
      pushAll enemyMsgs
      pure a
    _ -> CityOfBlood <$> runMessage msg attrs
