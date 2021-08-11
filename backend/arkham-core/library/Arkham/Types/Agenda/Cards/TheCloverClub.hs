module Arkham.Types.Agenda.Cards.TheCloverClub
  ( TheCloverClub(..)
  , theCloverClub
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Keyword
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.ScenarioId
import Arkham.Types.Target
import Arkham.Types.Trait

newtype TheCloverClub = TheCloverClub AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCloverClub :: AgendaCard TheCloverClub
theCloverClub = agenda (1, A) TheCloverClub Cards.theCloverClub (Static 4)

instance HasActions TheCloverClub

instance (HasSet EnemyId env (), HasSet Trait env EnemyId) => HasModifiersFor env TheCloverClub where
  getModifiersFor _ (EnemyTarget eid) (TheCloverClub attrs) | onSide A attrs =
    do
      enemyIds <- getSet ()
      if eid `member` enemyIds
        then do
          traits <- getSet eid
          pure $ toModifiers
            attrs
            [ AddKeyword Aloof | Criminal `member` traits ]
        else pure []
  getModifiersFor _ _ _ = pure []

instance AgendaRunner env => RunMessage env TheCloverClub where
  runMessage msg a@(TheCloverClub attrs@AgendaAttrs {..}) = case msg of
    InvestigatorDamageEnemy _ eid | agendaSequence == Agenda 1 A -> do
      traits <- getSet eid
      a <$ when (Criminal `member` traits) (push $ AdvanceAgenda agendaId)
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      completedExtracurricularActivity <-
        elem "02041" . map unCompletedScenarioId <$> getSetList ()
      enemyIds <- getSetList Criminal

      let
        continueMessages =
          [ShuffleEncounterDiscardBackIn, NextAgenda aid "02064"]
            <> [ AdvanceCurrentAgenda | completedExtracurricularActivity ]

      a <$ pushAll
        (map EnemyCheckEngagement enemyIds
        <> [chooseOne leadInvestigatorId [Label "Continue" continueMessages]]
        )
    _ -> TheCloverClub <$> runMessage msg attrs
