module Arkham.Scenarios.ALightInTheFog.Helpers.Location (module X, module Arkham.Scenarios.ALightInTheFog.Helpers.Location) where

import Arkham.Classes.Entity
import Arkham.Classes.RunMessage.Internal
import Arkham.GameT
import Arkham.Location.Grid
import Arkham.Location.Runner ()
import Arkham.Location.Types (LocationAttrs, connectedMatchersL, revealedConnectedMatchersL)
import Arkham.Matcher
import Arkham.Message (Message (PlaceGrid))
import Arkham.Prelude
import Arkham.Scenarios.ALightInTheFog.Helpers as X
import Control.Monad.Trans

setConnectedInRow
  :: (MonadTrans t, Entity a, EntityAttrs a ~ LocationAttrs) => Pos -> a -> t GameT a
setConnectedInRow pos l = do
  let attrs = toAttrs l
  attrs' <- liftRunMessage (PlaceGrid (GridLocation pos attrs.id)) attrs
  pure
    $ overAttrs
      ( const
          ( attrs'
              & (connectedMatchersL <>~ [LocationInRow pos.row])
              . (revealedConnectedMatchersL <>~ [LocationInRow pos.row])
          )
      )
      l
