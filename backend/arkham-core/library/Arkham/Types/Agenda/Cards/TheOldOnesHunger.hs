module Arkham.Types.Agenda.Cards.TheOldOnesHunger
  ( TheOldOnesHunger(..)
  , theOldOnesHunger
  )
where

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

newtype TheOldOnesHunger = TheOldOnesHunger AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOldOnesHunger :: TheOldOnesHunger
theOldOnesHunger = TheOldOnesHunger
  $ baseAttrs "02197" "The Old Ones Hunger" (Agenda 2 A) (Static 6)

instance HasModifiersFor env TheOldOnesHunger where
  getModifiersFor = noModifiersFor

instance HasActions env TheOldOnesHunger where
  getActions i window (TheOldOnesHunger x) = getActions i window x

instance AgendaRunner env => RunMessage env TheOldOnesHunger where
  runMessage msg a@(TheOldOnesHunger attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      scenarioDeckCount <- unScenarioDeckCount <$> getCount ()
      if scenarioDeckCount >= 2
        then a <$ unshiftMessages
          [ UseScenarioSpecificAbility leadInvestigatorId 1
          , NextAgenda agendaId "02198"
          ]
        else a <$ unshiftMessage (NextAgenda agendaId "02198")
    _ -> TheOldOnesHunger <$> runMessage msg attrs
