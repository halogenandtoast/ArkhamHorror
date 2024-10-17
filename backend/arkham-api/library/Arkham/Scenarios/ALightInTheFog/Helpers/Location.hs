module Arkham.Scenarios.ALightInTheFog.Helpers.Location (module X, module Arkham.Scenarios.ALightInTheFog.Helpers.Location) where

import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.Query (select)
import Arkham.Classes.RunMessage.Internal
import Arkham.GameT
import Arkham.Helpers.Modifiers (Modifier, ModifierType (DoNotDrawConnection), maybeModified)
import Arkham.Location.Grid
import Arkham.Location.Runner ()
import Arkham.Location.Types (LocationAttrs, connectedMatchersL, revealedConnectedMatchersL)
import Arkham.Matcher
import Arkham.Message (Message (PlaceGrid))
import Arkham.Prelude
import Arkham.Scenarios.ALightInTheFog.Helpers as X
import Arkham.SortedPair
import Arkham.Target
import Control.Monad.Trans

preventDrawConnections :: HasGame m => Target -> LocationAttrs -> m [Modifier]
preventDrawConnections target attrs = maybeModified attrs do
  guard $ isTarget attrs target
  pos <- hoistMaybe attrs.position
  connected <- lift $ select $ LocationInRow pos.row
  pure [DoNotDrawConnection $ sortedPair attrs.id connectedId | connectedId <- connected]

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
