module Arkham.Tracing where

import Arkham.Prelude
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
-- import OpenTelemetry.Trace qualified as Trace
-- import OpenTelemetry.Trace.Monad (inSpan')

class Tracing m where
  type SpanType m
  type SpanArgs m
  doTrace :: Text -> SpanArgs m -> (SpanType m -> m a) -> m a
  defaultSpanArgs :: SpanArgs m

instance Tracing m => Tracing (MaybeT m) where
  type SpanType (MaybeT m) = SpanType m
  type SpanArgs (MaybeT m) = SpanArgs m
  defaultSpanArgs = defaultSpanArgs @m
  doTrace name args action = MaybeT
    $ doTrace name args
    $ \sp -> do
      runMaybeT (action sp)

instance Tracing m => Tracing (ReaderT r m) where
  type SpanType (ReaderT r m) = SpanType m
  type SpanArgs (ReaderT r m) = SpanArgs m
  defaultSpanArgs = defaultSpanArgs @m
  doTrace name args action = ReaderT $ \r ->
    doTrace name args $ \sp -> runReaderT (action sp) r

instance (Monad m, Tracing m) => Tracing (StateT s m) where
  type SpanType (StateT s m) = SpanType m
  type SpanArgs (StateT s m) = SpanArgs m
  defaultSpanArgs = defaultSpanArgs @m
  doTrace name args action = StateT $ \s ->
    doTrace name args $ \sp -> do
      (a, s') <- runStateT (action sp) s
      pure (a, s')

instance Tracing m => Tracing (WriterT w m) where
  type SpanType (WriterT w m) = SpanType m
  type SpanArgs (WriterT w m) = SpanArgs m
  defaultSpanArgs = defaultSpanArgs @m
  doTrace name args action = WriterT
    $ doTrace name args
    $ \sp -> do
      runWriterT (action sp)

withSpan_ :: forall m a. Tracing m => Text -> m a -> m a
withSpan_ name action = doTrace name (defaultSpanArgs @m) (const action)

withSpan' :: forall m a. Tracing m => Text -> (SpanType m -> m a) -> m a
withSpan' name action = doTrace name (defaultSpanArgs @m) action
