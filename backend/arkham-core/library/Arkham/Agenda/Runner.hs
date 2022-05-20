{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Agenda.Runner (module Arkham.Agenda.Runner, module X) where

import Arkham.Prelude

import Arkham.ActId
import Arkham.Agenda.AdvancementReason
import Arkham.Agenda.Attrs as X
import Arkham.Agenda.Sequence as X
import Arkham.Act.Attrs (ActAttrs)
import Arkham.Card
import Arkham.Classes
import Arkham.Direction
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message
import Arkham.Name
import Arkham.Query
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

type AgendaRunner env =
  ( HasQueue env
  , HasRecord env ()
  , Query AssetMatcher env
  , Query EnemyMatcher env
  , Query ExtendedCardMatcher env
  , Query InvestigatorMatcher env
  , Query LocationMatcher env
  , Query ActMatcher env
  , Projection env ActAttrs
  , HasCount ClueCount env InvestigatorId
  , HasCount ClueCount env LocationId
  , HasCount DiscardCount env InvestigatorId
  , HasCount DoomCount env ()
  , HasCount PlayerCount env ()
  , HasCount ScenarioDeckCount env ScenarioDeckKey
  , HasCount SetAsideCount env CardCode
  , HasId (Maybe LocationId) env LocationMatcher
  , HasId (Maybe LocationId) env (Direction, LocationId)
  , HasId (Maybe StoryTreacheryId) env CardCode
  , HasId CardCode env EnemyId
  , HasId LeadInvestigatorId env ()
  , HasId LocationId env InvestigatorId
  , HasList LocationName env ()
  , HasList (InvestigatorId, Distance) env EnemyMatcher
  , HasList (LocationId, Distance) env InvestigatorId
  , HasSet ActId env ()
  , HasSet ClosestPathLocationId env (LocationId, LocationId)
  , HasSet ClosestPathLocationId env (LocationId, LocationMatcher)
  , HasSet CompletedScenarioId env ()
  , HasSet EnemyId env ()
  , HasSet EnemyId env ([Trait], LocationId)
  , HasSet EnemyId env LocationId
  , HasSet EnemyId env EnemyMatcher
  , HasSet EnemyId env LocationMatcher
  , HasSet EnemyId env Trait
  , HasSet InScenarioInvestigatorId env ()
  , HasSet InvestigatorId env ()
  , HasSet InvestigatorId env EnemyId
  , HasSet InvestigatorId env LocationMatcher
  , HasSet InvestigatorId env LocationId
  , HasSet LocationId env ()
  , HasSet LocationId env [Trait]
  , HasSet Trait env EnemyId
  )

instance
  ( HasQueue env
  , HasCount DoomCount env ()
  , HasCount PlayerCount env ()
  , HasId LeadInvestigatorId env ()
  , HasSet InvestigatorId env ()
  )
  => RunMessage env AgendaAttrs
  where
  runMessage msg a@AgendaAttrs {..} = case msg of
    PlaceUnderneath target cards | isTarget a target ->
      pure $ a & cardsUnderneathL %~ (<> cards)
    PlaceDoom (AgendaTarget aid) n | aid == agendaId -> pure $ a & doomL +~ n
    Discard (TreacheryTarget tid) -> pure $ a & treacheriesL %~ deleteSet tid
    AttachTreachery tid (AgendaTarget aid) | aid == agendaId ->
      pure $ a & treacheriesL %~ insertSet tid
    AdvanceAgenda aid | aid == agendaId && agendaSide agendaSequence == A -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push $ chooseOne leadInvestigatorId [AdvanceAgenda agendaId]
      pure
        $ a
        & (sequenceL .~ Agenda (unAgendaStep $ agendaStep agendaSequence) B)
        & (flippedL .~ True)
    AdvanceAgendaIfThresholdSatisfied -> do
      perPlayerDoomThreshold <- getPlayerCountValue (a ^. doomThresholdL)
      totalDoom <- unDoomCount <$> getCount ()
      when
        (totalDoom >= perPlayerDoomThreshold)
        do
          whenMsg <- checkWindows
            [ Window
                Timing.When
                (Window.AgendaWouldAdvance DoomThreshold $ toId a)
            ]
          afterMsg <- checkWindows
            [ Window
                Timing.After
                (Window.AgendaWouldAdvance DoomThreshold $ toId a)
            ]
          pushAll [whenMsg, afterMsg, Do AdvanceAgendaIfThresholdSatisfied]
      pure a
    Do AdvanceAgendaIfThresholdSatisfied -> do
      -- This status can change due to the above windows so we much check again
      perPlayerDoomThreshold <- getPlayerCountValue (a ^. doomThresholdL)
      totalDoom <- unDoomCount <$> getCount ()
      when (totalDoom >= perPlayerDoomThreshold) $ do
        leadInvestigatorId <- getLeadInvestigatorId
        pushAll
          [ CheckWindow
            [leadInvestigatorId]
            [Window Timing.When (Window.AgendaAdvance agendaId)]
          , AdvanceAgenda agendaId
          , RemoveAllDoom
          ]
      pure a
    RemoveAllDoom -> do
      pure $ a & doomL .~ 0
    RevertAgenda aid | aid == agendaId && onSide B a ->
      pure
        $ a
        & (sequenceL .~ Agenda (unAgendaStep $ agendaStep agendaSequence) A)
    _ -> pure a
