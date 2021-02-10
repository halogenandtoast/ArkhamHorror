module Arkham.Types.Agenda.Cards.VengeanceAwaits where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


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
