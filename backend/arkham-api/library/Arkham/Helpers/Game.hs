module Arkham.Helpers.Game (module Arkham.Helpers.Game, module X) where

import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.Game as X
import {-# SOURCE #-} Arkham.GameEnv as X
import Arkham.Prelude
import Control.Monad.Reader (local)

withDepthGuard :: HasGame m => Int -> a -> ReaderT Game m a -> m a
withDepthGuard maxDepth defaultValue body = do
  game <- getGame
  flip runReaderT game $ do
    depth <- getDepthLock
    if depth > maxDepth then pure defaultValue else local delve body

withAlteredGame :: HasGame m => (Game -> Game) -> ReaderT Game m a -> m a
withAlteredGame f body = do
  game <- getGame
  runReaderT body (f game)
