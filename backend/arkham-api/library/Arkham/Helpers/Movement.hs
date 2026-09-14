module Arkham.Helpers.Movement where

import Arkham.Classes.HasQueue
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Message
import Arkham.Message.Lifted
import Arkham.Movement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Window (Window (..), WindowType (..), getBatchId, replaceWindowType)
import Control.Monad.Trans.Class

{- | Retarget an in-flight move. The movement slot is what 'ResolveMovement'
reads, so setting it is what actually changes where the investigator lands; the
queue rewrite catches the messages that were built from the old destination
before the redirect happened.
-}
replaceMovement
  :: (MonadTrans t, HasQueue Message m, ReverseQueue (t m))
  => InvestigatorId -> (Movement -> Movement) -> t m ()
replaceMovement iid f =
  field InvestigatorMovement iid >>= traverse_ \movement -> do
    let
      movement' = f movement
      newDestination = case movement'.destination of
        ToLocation lid -> Just lid
        ToLocationMatching _ -> Nothing
      -- 'Entering' and 'Moves' windows are built when the move is expanded, so
      -- a redirect leaves them naming the location we are no longer going to.
      staleDestination = case (movement.destination, newDestination) of
        (ToLocation old, Just new) | old /= new -> Just old
        _ -> Nothing
      isStaleWindow w = case (staleDestination, w.windowType) of
        (Just old, Entering iid' lid) -> iid' == iid && lid == old
        (Just old, Moves iid' _ _ lid mid) -> iid' == iid && lid == old && mid == movement.id
        _ -> False
      retargetWindow w = case (newDestination, w.windowType) of
        (Just new, Entering iid' _) -> replaceWindowType (Entering iid' new) w
        (Just new, Moves iid' source from _ mid) -> replaceWindowType (Moves iid' source from new mid) w
        _ -> w
      isMovement = \case
        Would _ msgs -> any isMovement msgs
        WhenCanMove _ msgs -> any isMovement msgs
        MoveWithSkillTest msg -> isMovement msg
        CheckWindows ws -> any isStaleWindow ws
        MoveTo m -> movement.id == m.id
        _ -> False
      replace = \case
        Would bid msgs -> Would bid $ map replace msgs
        WhenCanMove bid msgs -> WhenCanMove bid $ map replace msgs
        MoveWithSkillTest msg -> MoveWithSkillTest $ replace msg
        CheckWindows ws -> CheckWindows $ map (\w -> if isStaleWindow w then retargetWindow w else w) ws
        MoveTo m | movement.id == m.id -> MoveTo movement'
        other -> other
    insteadOfMatchingWith isMovement (pure . pure . replace)
    priority $ push $ SetMovement iid movement'

cancelEnemyMovement :: ReverseQueue m => enemy -> m ()
cancelEnemyMovement _enemy =
  getWindowStack >>= \case
    [] -> pure ()
    (ws : _) -> cancelBatch $ getBatchId ws
