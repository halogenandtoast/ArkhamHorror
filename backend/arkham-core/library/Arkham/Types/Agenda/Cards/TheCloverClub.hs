module Arkham.Types.Agenda.Cards.TheCloverClub
  ( TheCloverClub(..)
  , theCloverClub
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
import Arkham.Types.Game.Helpers
import Arkham.Types.Keyword
import Arkham.Types.Trait

newtype TheCloverClub = TheCloverClub AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCloverClub :: TheCloverClub
theCloverClub =
  TheCloverClub $ baseAttrs "02063" "The Clover Club" (Agenda 1 A) (Static 4)

instance HasActions env TheCloverClub where
  getActions i window (TheCloverClub x) = getActions i window x

instance HasSet Trait env EnemyId => HasModifiersFor env TheCloverClub where
  getModifiersFor _ (EnemyTarget eid) (TheCloverClub attrs) | onSide A attrs =
    do
      traits <- getSet eid
      pure $ toModifiers attrs [ AddKeyword Aloof | Criminal `member` traits ]
  getModifiersFor _ _ _ = pure []

instance AgendaRunner env => RunMessage env TheCloverClub where
  runMessage msg a@(TheCloverClub attrs@AgendaAttrs {..}) = case msg of
    InvestigatorDamageEnemy _ eid | agendaSequence == Agenda 1 A -> do
      traits <- getSet eid
      a <$ when
        (Criminal `member` traits)
        (unshiftMessage $ AdvanceAgenda agendaId)
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      completedExtracurricularActivity <-
        elem "02041" . map unCompletedScenarioId <$> getSetList ()
      enemyIds <- getSetList Criminal

      let
        continueMessages =
          [ShuffleEncounterDiscardBackIn, NextAgenda aid "02064"]
            <> [ AdvanceCurrentAgenda | completedExtracurricularActivity ]

      a <$ unshiftMessages
        (map EnemyCheckEngagement enemyIds
        <> [chooseOne leadInvestigatorId [Label "Continue" continueMessages]]
        )
    _ -> TheCloverClub <$> runMessage msg attrs
