module Arkham.Types.Agenda.Cards.VengeanceAwaits where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Target
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner

newtype VengeanceAwaits = VengeanceAwaits AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengeanceAwaits :: VengeanceAwaits
vengeanceAwaits =
  VengeanceAwaits $ baseAttrs "01145" "Vengeance Awaits" (Agenda 3 A) (Static 5)

instance HasModifiersFor env VengeanceAwaits where
  getModifiersFor = noModifiersFor

instance HasActions env VengeanceAwaits where
  getActions i window (VengeanceAwaits x) = getActions i window x

instance AgendaRunner env => RunMessage env VengeanceAwaits where
  runMessage msg a@(VengeanceAwaits attrs@AgendaAttrs {..}) = case msg of
    EnemyDefeated _ _ _ "01156" _ _ ->
      a <$ unshiftMessage (ScenarioResolution $ Resolution 2)
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 B -> do
      actIds <- getSetList ()
      umordhoth <- EncounterCard <$> genEncounterCard "01157"
      a <$ if "01146" `elem` actIds
        then
          unshiftMessages
          $ [PlaceLocation "01156", CreateEnemyAt umordhoth "01156"]
          <> [ Discard (ActTarget actId) | actId <- actIds ]
        else do
          enemyIds <- getSetList (LocationId "01156")
          unshiftMessages
            $ [ Discard (EnemyTarget eid) | eid <- enemyIds ]
            <> [CreateEnemyAt umordhoth "01156"]
            <> [ Discard (ActTarget actId) | actId <- actIds ]
    _ -> VengeanceAwaits <$> runMessage msg attrs
