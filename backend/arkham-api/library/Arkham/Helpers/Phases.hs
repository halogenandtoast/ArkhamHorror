module Arkham.Helpers.Phases where

import Arkham.Classes.HasQueue
import {-# SOURCE #-} Arkham.GameEnv (getPhase)
import Arkham.Helpers.Window (checkWindows)
import Arkham.Message (Message)
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted hiding (checkWindows)
import Arkham.Phase
import Arkham.Prelude
import Arkham.Queue
import Arkham.Window qualified as Window

runEnemyPhase :: ReverseQueue m => Message -> m ()
runEnemyPhase endMsg = do
  phaseBeginsWindow <-
    checkWindows
      [ Window.mkWhen Window.AnyPhaseBegins
      , Window.mkWhen (Window.PhaseBegins EnemyPhase)
      , Window.mkAfter Window.AnyPhaseBegins
      , Window.mkAfter (Window.PhaseBegins EnemyPhase)
      ]
  enemiesAttackWindow <- checkWindows [Window.mkWhen Window.EnemiesAttackStep]
  afterHuntersMoveWindow <- checkWindows [Window.mkAfter Window.HuntersMoveStep]
  fastWindow <- checkWindows [Window.mkWhen Window.FastPlayerWindow]
  let phaseStep step msgs = Msg.PhaseStep (EnemyPhaseStep step) msgs
  pushAll
    [ phaseStep EnemyPhaseBeginsStep [phaseBeginsWindow]
    , phaseStep HunterEnemiesMoveStep [Msg.HuntersMove, afterHuntersMoveWindow]
    , phaseStep ResolveAttacksWindow [fastWindow, enemiesAttackWindow]
    , phaseStep ResolveAttacksStep [Msg.EnemiesAttack, fastWindow]
    , phaseStep EnemyPhaseEndsStep [endMsg]
    ]

withPhase :: ReverseQueue m => Phase -> QueueT Message m () -> m ()
withPhase phase body = do
  current <- getPhase
  msgs <- capture body
  pushAll $ Msg.SetPhase phase : msgs ++ [Msg.SetPhase current]
