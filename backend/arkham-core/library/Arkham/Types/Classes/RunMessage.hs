{-# LANGUAGE DefaultSignatures #-}
module Arkham.Types.Classes.RunMessage where

import ClassyPrelude

import {-# SOURCE #-} Arkham.Types.Message
import Control.Monad.Fail
import GHC.Generics
import Lens.Micro hiding (to)
import Lens.Micro.Extras

class HasQueue a where
  messageQueue :: Lens' a (IORef [Message])

class (HasQueue env) => RunMessage1 env f where
  runMessage1 :: (MonadIO m, MonadReader env m, MonadFail m) => Message -> f p -> m (f p)

instance (HasQueue env, RunMessage1 env f) => RunMessage1 env (M1 i c f) where
  runMessage1 msg (M1 x) = M1 <$> runMessage1 msg x

instance (HasQueue env, RunMessage1 env l, RunMessage1 env r) => RunMessage1 env (l :+: r) where
  runMessage1 msg (L1 x) = L1 <$> runMessage1 msg x
  runMessage1 msg (R1 x) = R1 <$> runMessage1 msg x

instance (HasQueue env, RunMessage env p) => RunMessage1 env (K1 R p) where
  runMessage1 msg (K1 x) = K1 <$> runMessage msg x

class (HasQueue env) => RunMessage env a where
  runMessage :: (MonadIO m, MonadReader env m, MonadFail m) => Message -> a -> m a
  default runMessage :: (Generic a, RunMessage1 env (Rep a), MonadIO m, MonadReader env m, MonadFail m) => Message -> a -> m a
  runMessage = defaultRunMessage

defaultRunMessage
  :: ( Generic a
     , RunMessage1 env (Rep a)
     , MonadIO m
     , MonadReader env m
     , MonadFail m
     )
  => Message
  -> a
  -> m a
defaultRunMessage msg = fmap to . runMessage1 msg . from

withQueue
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => ([Message] -> ([Message], r))
  -> m r
withQueue body = do
  ref <- asks $ view messageQueue
  liftIO $ atomicModifyIORef' ref body

popMessage :: (MonadIO m, MonadReader env m, HasQueue env) => m (Maybe Message)
popMessage = withQueue $ \case
  [] -> ([], Nothing)
  (m : ms) -> (ms, Just m)

clearQueue :: (MonadIO m, MonadReader env m, HasQueue env) => m ()
clearQueue = withQueue $ const ([], ())

peekMessage :: (MonadIO m, MonadReader env m, HasQueue env) => m (Maybe Message)
peekMessage = withQueue $ \case
  [] -> ([], Nothing)
  (m : ms) -> (m : ms, Just m)

pushMessage :: (MonadIO m, MonadReader env m, HasQueue env) => Message -> m ()
pushMessage = pushMessages . pure

pushMessages
  :: (MonadIO m, MonadReader env m, HasQueue env) => [Message] -> m ()
pushMessages msgs = withQueue $ \queue -> (queue <> msgs, ())

unshiftMessage
  :: (MonadIO m, MonadReader env m, HasQueue env) => Message -> m ()
unshiftMessage = unshiftMessages . pure

unshiftMessages
  :: (MonadIO m, MonadReader env m, HasQueue env) => [Message] -> m ()
unshiftMessages msgs = withQueue $ \queue -> (msgs <> queue, ())

