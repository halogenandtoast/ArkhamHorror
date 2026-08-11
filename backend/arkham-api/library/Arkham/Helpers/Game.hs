module Arkham.Helpers.Game (module Arkham.Helpers.Game, module X) where

import Arkham.Card (Card)
import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.Game as X
import {-# SOURCE #-} Arkham.GameEnv as X
import Arkham.Modifier.Builder (CacheReaderT, runCacheReaderT)
import Arkham.Prelude

{- | Every card that has been removed from the game.

@RemoveFromGame@ deletes the entity (see @RemoveAsset@ in @Arkham.Game.Runner@),
so a removed asset/event/skill survives only as its card here — there is no
matcher that will find it. Scenarios whose rules keep referring to removed cards
(Dark Matter's "Cards Removed from the Game" area, for instance) have to read
this list.
-}
getRemovedFromPlayCards :: HasGame m => m [Card]
getRemovedFromPlayCards = gameRemovedFromPlay <$> getGame

{- | Run @body@ with the game's recursion depth incremented, bailing out with
@defaultValue@ once @maxDepth@ is exceeded.

The body runs under 'CacheReaderT' rather than a plain @ReaderT Game m@ so it
keeps delegating to the surrounding query cache. This is the hot path for
@PerformableAbility@ criteria (e.g. Eon Chart's "any performable move/
investigate/evade action"), which run their entire grid-accessibility check
inside this guard: under the old passthrough @ReaderT Game@ every one of
those checks re-scanned the board uncached, so the fast window timed out on
grid scenarios (issue #4985). 'CacheReaderT' has the same instance
requirements as @ReaderT Game@, so the swap is constraint-neutral.
-}
withDepthGuard :: HasGame m => Int -> a -> CacheReaderT m a -> m a
withDepthGuard maxDepth defaultValue body = do
  depth <- getDepthLock
  if depth > maxDepth
    then pure defaultValue
    else do
      g <- getGame
      runCacheReaderT (delve g) body

withAlteredGame :: HasGame m => (Game -> Game) -> ReaderT Game m a -> m a
withAlteredGame f body = getGame >>= runReaderT body . f
