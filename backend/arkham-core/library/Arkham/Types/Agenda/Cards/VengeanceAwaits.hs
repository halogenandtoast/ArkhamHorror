module Arkham.Types.Agenda.Cards.VengeanceAwaits where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import qualified Arkham.Enemy.Cards as Enemies
import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Target

newtype VengeanceAwaits = VengeanceAwaits AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengeanceAwaits :: AgendaCard VengeanceAwaits
vengeanceAwaits =
  agenda (3, A) VengeanceAwaits Cards.vengeanceAwaits (Static 5)

instance HasModifiersFor env VengeanceAwaits
instance HasActions VengeanceAwaits

instance AgendaRunner env => RunMessage env VengeanceAwaits where
  runMessage msg a@(VengeanceAwaits attrs@AgendaAttrs {..}) = case msg of
    EnemyDefeated _ _ _ "01157" _ _ ->
      a <$ push (ScenarioResolution $ Resolution 2)
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 B -> do
      actIds <- getSetList ()
      umordhoth <- EncounterCard <$> genEncounterCard Enemies.umordhoth
      a <$ if "01146" `elem` actIds
        then do
          ritualSiteId <- getRandom
          pushAll
            $ [ PlaceLocation ritualSiteId Locations.ritualSite
              , CreateEnemyAt umordhoth ritualSiteId Nothing
              ]
            <> [ Discard (ActTarget actId) | actId <- actIds ]
        else do
          ritualSiteId <- getJustLocationIdByName "Ritual Site"
          enemyIds <- getSetList ritualSiteId
          pushAll
            $ [ Discard (EnemyTarget eid) | eid <- enemyIds ]
            <> [CreateEnemyAt umordhoth ritualSiteId Nothing]
            <> [ Discard (ActTarget actId) | actId <- actIds ]
    _ -> VengeanceAwaits <$> runMessage msg attrs
