module Arkham.Helpers.Movement where

import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Message
import Arkham.Message.Lifted
import Arkham.Movement
import Arkham.Prelude
import Arkham.Projection
import Control.Monad.Trans.Class

replaceMovement
  :: (MonadTrans t, HasQueue Message m, HasGame (t m))
  => InvestigatorId -> (Movement -> Movement) -> t m ()
replaceMovement iid f =
  field InvestigatorMovement iid >>= traverse_ \movement -> do
    let
      isMovement = \case
        Would _ msgs -> any isMovement msgs
        WhenCanMove _ msgs -> any isMovement msgs
        MoveTo m -> movement.id == m.id
        _ -> False
      replace = \case
        Would bid msgs -> Would bid $ map replace msgs
        WhenCanMove bid msgs -> WhenCanMove bid $ map replace msgs
        MoveTo m | movement.id == m.id -> MoveTo $ f movement
        other -> other
    insteadOfMatchingWith isMovement (pure . pure . replace)
