{- | A dedicated, /pure/ monad for the single 'preloadModifiers' pass.

'preloadModifiers' rebuilds the entire modifier map after (nearly) every
message by running every entity's 'getModifiersFor'. Almost all of those
implementations issue @select@/@field@ queries, and many entities of the
same card type (e.g. a large swarm) issue the /same/ query. Run under the
usual @WriterT (ReaderT Game GameT)@ stack those queries hit the no-op caches
on the transformer layers, so each one re-scans the whole game from scratch —
turning a single pass into O(entities × query-fanout) with zero
de-duplication.

'ModifierBuilder' carries:

  * the immutable game @g@ the pass reads from ('getGame'), and
  * a scoped query cache ('getCache') backed by plain 'State'

so identical @select@/@selectExists@ queries issued by different entities in
the same pass are computed once and reused. Because @g@ never changes during
the pass the cached answers are bit-identical to recomputing them, so this is
purely a performance change — the rules are unaffected. The cache lives only
for the duration of one 'buildModifiers' call and is then discarded.

This lives in its own module (rather than alongside 'HasModifiersFor') so
that touching it does not force a recompile of the entire card library.
-}
module Arkham.Modifier.Builder (
  ModifierBuilder,
  buildModifiers,
  runCachedQuery,
  CachedT,
  runCachedQueryT,
  CacheReaderT,
  runCacheReaderT,
) where

import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.Game.Base (Game)
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Target
import Arkham.Tracing
import Control.Monad.State.Strict (
  State,
  StateT,
  evalState,
  evalStateT,
  execState,
  get,
  gets,
  modify',
 )
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

{- | 'addAttribute'/'doTrace' are no-ops: the modifier pass does no tracing and
this keeps 'ModifierBuilder' free of any OpenTelemetry/metrics machinery.
-}
instance Tracing ModifierBuilder where
  type SpanType ModifierBuilder = ()
  type SpanArgs ModifierBuilder = ()
  defaultSpanArgs = ()
  addAttribute _ _ _ = pure ()
  doTrace _ _ action = action ()

{- | Run a modifier-collecting computation against a fixed game with a scoped
query cache, returning the accumulated modifiers. Drop-in replacement for
@execWriterT@ over the old transformer stack.
-}
buildModifiers :: Game -> ModifierBuilder () -> MonoidalMap Target [Modifier]
buildModifiers g (ModifierBuilder r) =
  psSink (execState (runReaderT r g) (PreloadState DMap.empty mempty))

{- | Run a read-only query (e.g. 'getActions') against a fixed game with a
scoped query cache, returning its result. Any modifier writes are discarded.

The usual @ReaderT Game GameT@ query stack (as entered by 'asIfTurn') hits the
no-op caches on its transformer layers, so identical @select@/@getAllAbilities@
queries — e.g. a @PerformableAbility@ criterion re-enumerating every ability —
re-scan the whole game each time. Running the query here computes each such key
once. Because @g@ is frozen for the pass the cached answers are bit-identical to
recomputing them, so this is purely a performance change.
-}
runCachedQuery :: Game -> ModifierBuilder a -> a
runCachedQuery g (ModifierBuilder r) =
  evalState (runReaderT r g) (PreloadState DMap.empty mempty)

{- | A scoped query cache layered over an arbitrary base monad @m@, preserving
@m@'s tracing (unlike 'ModifierBuilder', which no-ops tracing). Run a
read-only query (e.g. 'getActions') under 'runCachedQueryT' to memoize
identical @select@/@getAllAbilities@/accessibility queries against a frozen
game while still recording metrics through the base monad.
-}
newtype CachedT m a = CachedT (StateT (DMap CacheKey Identity) m a)
  deriving newtype (Functor, Applicative, Monad)

instance HasGame m => HasGame (CachedT m) where
  getGame = CachedT (lift getGame)
  getCache = GameCache \k (CachedT build) -> CachedT do
    c <- get
    case DMap.lookup k c of
      Just v -> pure (runIdentity v)
      Nothing -> do
        !v <- build
        modify' (DMap.insert k (Identity v))
        pure v

instance (Monad m, Tracing m) => Tracing (CachedT m) where
  type SpanType (CachedT m) = SpanType m
  type SpanArgs (CachedT m) = SpanArgs m
  defaultSpanArgs = defaultSpanArgs @m
  addAttribute sp key value = CachedT (lift (addAttribute sp key value))
  doTrace name args action = CachedT $ doTrace name args \sp ->
    case action sp of CachedT st -> st

runCachedQueryT :: Monad m => CachedT m a -> m a
runCachedQueryT (CachedT s) = evalStateT s DMap.empty

{- | Like 'CachedT' but /delegates/ its cache to the base monad @m@ instead of
opening a fresh one, while overriding the game seen by 'getGame' to a fixed
@g@. Used by 'getLocationsMatching' for the @IncludeEmptySpace@ (grid
movement) branch: it needs @gameAllowEmptySpaces = True@ to propagate to
nested selects through the game env, yet must keep sharing the surrounding
pass's live cache so the repeated grid-accessibility queries dedupe across
sibling criteria checks. Soundness against the flag=False world is guaranteed
by 'cached', which namespaces keys with 'NamespaceEmptyKey' whenever the flag
is set — so the delegated entries never collide with ordinary results.
-}
newtype CacheReaderT m a = CacheReaderT (ReaderT Game m a)
  deriving newtype (Functor, Applicative, Monad)

instance HasGame m => HasGame (CacheReaderT m) where
  getGame = CacheReaderT ask
  getCache = GameCache \k (CacheReaderT rt) ->
    CacheReaderT $ ReaderT \g -> case getCache @m of
      GameCache wc -> wc k (runReaderT rt g)

instance Tracing m => Tracing (CacheReaderT m) where
  type SpanType (CacheReaderT m) = SpanType m
  type SpanArgs (CacheReaderT m) = SpanArgs m
  defaultSpanArgs = defaultSpanArgs @m
  addAttribute sp key value = CacheReaderT $ ReaderT \_ -> addAttribute sp key value
  doTrace name args action = CacheReaderT $ ReaderT \r ->
    doTrace name args \sp -> case action sp of CacheReaderT rt -> runReaderT rt r

runCacheReaderT :: Game -> CacheReaderT m a -> m a
runCacheReaderT g (CacheReaderT r) = runReaderT r g
