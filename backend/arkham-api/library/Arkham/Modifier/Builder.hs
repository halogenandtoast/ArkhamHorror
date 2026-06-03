-- | A dedicated, /pure/ monad for the single 'preloadModifiers' pass.
--
-- 'preloadModifiers' rebuilds the entire modifier map after (nearly) every
-- message by running every entity's 'getModifiersFor'. Almost all of those
-- implementations issue @select@/@field@ queries, and many entities of the
-- same card type (e.g. a large swarm) issue the /same/ query. Run under the
-- usual @WriterT (ReaderT Game GameT)@ stack those queries hit the no-op caches
-- on the transformer layers, so each one re-scans the whole game from scratch —
-- turning a single pass into O(entities × query-fanout) with zero
-- de-duplication.
--
-- 'ModifierBuilder' carries:
--
--   * the immutable game @g@ the pass reads from ('getGame'), and
--   * a scoped query cache ('getCache') backed by plain 'State'
--
-- so identical @select@/@selectExists@ queries issued by different entities in
-- the same pass are computed once and reused. Because @g@ never changes during
-- the pass the cached answers are bit-identical to recomputing them, so this is
-- purely a performance change — the rules are unaffected. The cache lives only
-- for the duration of one 'buildModifiers' call and is then discarded.
--
-- This lives in its own module (rather than alongside 'HasModifiersFor') so
-- that touching it does not force a recompile of the entire card library.
module Arkham.Modifier.Builder (ModifierBuilder, buildModifiers) where

import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.Game.Base (Game)
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Target
import Arkham.Tracing
import Control.Monad.State.Strict (State, execState, gets, modify')
import Control.Monad.Writer.Class
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Map.Monoidal.Strict

data PreloadState = PreloadState
  { psCache :: !(DMap CacheKey Identity)
  , psSink :: !(MonoidalMap Target [Modifier])
  }

newtype ModifierBuilder a = ModifierBuilder (ReaderT Game (State PreloadState) a)
  deriving newtype (Functor, Applicative, Monad)

instance HasGame ModifierBuilder where
  getGame = ModifierBuilder ask
  getCache = GameCache \k (ModifierBuilder build) -> ModifierBuilder do
    cache <- gets psCache
    case DMap.lookup k cache of
      Just v -> pure (runIdentity v)
      Nothing -> do
        v <- build
        modify' \s -> s {psCache = DMap.insert k (Identity v) (psCache s)}
        pure v

instance MonadWriter (MonoidalMap Target [Modifier]) ModifierBuilder where
  tell w = ModifierBuilder (modify' \s -> s {psSink = psSink s <> w})
  writer (a, w) = a <$ tell w
  listen (ModifierBuilder m) = ModifierBuilder do
    before <- gets psSink
    modify' \s -> s {psSink = mempty}
    a <- m
    w <- gets psSink
    modify' \s -> s {psSink = before <> w}
    pure (a, w)
  pass (ModifierBuilder m) = ModifierBuilder do
    before <- gets psSink
    modify' \s -> s {psSink = mempty}
    (a, f) <- m
    w <- gets psSink
    modify' \s -> s {psSink = before <> f w}
    pure a

-- | 'addAttribute'/'doTrace' are no-ops: the modifier pass does no tracing and
-- this keeps 'ModifierBuilder' free of any OpenTelemetry/metrics machinery.
instance Tracing ModifierBuilder where
  type SpanType ModifierBuilder = ()
  type SpanArgs ModifierBuilder = ()
  defaultSpanArgs = ()
  addAttribute _ _ _ = pure ()
  doTrace _ _ action = action ()

-- | Run a modifier-collecting computation against a fixed game with a scoped
-- query cache, returning the accumulated modifiers. Drop-in replacement for
-- @execWriterT@ over the old transformer stack.
buildModifiers :: Game -> ModifierBuilder () -> MonoidalMap Target [Modifier]
buildModifiers g (ModifierBuilder r) =
  psSink (execState (runReaderT r g) (PreloadState DMap.empty mempty))
