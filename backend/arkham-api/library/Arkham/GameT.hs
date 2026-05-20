module Arkham.GameT where

import Arkham.Classes.GameLogger
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import {-# SOURCE #-} Arkham.Game.Base
import {-# SOURCE #-} Arkham.Message
import Arkham.Metrics (isMetricsEnabled, recordSpan)
import GHC.Clock (getMonotonicTimeNSec)
import Arkham.Prelude
import Arkham.Queue
import Arkham.Random
import Arkham.Tracing
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Random
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import OpenTelemetry.Trace qualified as Trace
import OpenTelemetry.Trace.Monad (MonadTracer (..), inSpan')

data GameEnv = GameEnv
  { gameEnvGame :: IORef Game
  , gameEnvQueue :: Queue Message
  , gameRandomGen :: IORef StdGen
  , gameLogger :: ClientMessage -> IO ()
  , gameCacheRef :: IORef (DMap CacheKey Identity)
  , gameTracer :: Trace.Tracer
  }

newtype GameT a = GameT {unGameT :: ReaderT GameEnv IO a}
  deriving newtype
    ( MonadReader GameEnv
    , Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadMask
    , MonadCatch
    , MonadThrow
    )

instance MonadTracer GameT where
  getTracer = asks gameTracer

instance Tracing GameT where
  type SpanType GameT = Trace.Span
  type SpanArgs GameT = Trace.SpanArguments
  defaultSpanArgs = Trace.defaultSpanArguments
  -- `addAttribute` is intentionally a no-op: we have no OTel exporter wired
  -- up and the Text values fed to it (often `tshow` of a deep ADT) are
  -- non-trivial to compute. With no consumer the work is pure overhead.
  -- Since the argument is unused here, GHC's laziness keeps the thunk
  -- unforced.
  addAttribute _ _ _ = pure ()
  -- Fast path: when metrics are disabled AND no OTel exporter is wired up,
  -- skip `inSpan'` entirely. Every `withSpan_`/`withSpan'` call otherwise
  -- creates a real Span data structure (createSpan, context, bracketError),
  -- which dominates engine time for message-heavy steps. The Span argument
  -- is safe to leave unforced because every `addAttribute` impl ignores it.
  doTrace name args action = do
    mref <- liftIO isMetricsEnabled
    case mref of
      Nothing -> action (error "doTrace: span unused on fast path")
      Just _ -> do
        t0 <- liftIO getMonotonicTimeNSec
        inSpan' name args action `finally` do
          t1 <- liftIO getMonotonicTimeNSec
          liftIO $ recordSpan name (t1 - t0)
instance HasGame GameT where
  getGame = asks gameEnvGame >>= readIORef
  getCache = GameCache \k build -> do
    ref <- asks gameCacheRef
    dm <- liftIO $ readIORef ref
    case DMap.lookup k dm of
      Just v -> pure $ runIdentity v
      Nothing -> do
        !v <- build
        liftIO $ atomicModifyIORef' ref $ \dm' -> (DMap.insert k (Identity v) dm', ())
        pure v

instance HasStdGen GameEnv where
  genL = lens gameRandomGen $ \m x -> m {gameRandomGen = x}

instance HasQueue Message GameT where
  messageQueue = asks gameEnvQueue

instance HasGameLogger GameT where
  getLogger = do
    logger <- asks gameLogger
    pure $ \msg -> liftIO $ logger msg
