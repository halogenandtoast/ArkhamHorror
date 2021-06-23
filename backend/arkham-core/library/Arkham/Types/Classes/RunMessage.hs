{-# LANGUAGE DefaultSignatures #-}
module Arkham.Types.Classes.RunMessage
  ( module Arkham.Types.Classes.RunMessage
  )
where

import Arkham.Prelude hiding (to)

import Arkham.Types.Classes.HasQueue
import Arkham.Types.Message
import Control.Monad.State
import GHC.Generics

class RunMessage1 env f where
  runMessage1 :: (MonadIO m, MonadReader env m, MonadRandom m, HasQueue env) => Message -> f p -> m (f p)

instance RunMessage1 env f => RunMessage1 env (M1 i c f) where
  runMessage1 msg (M1 x) = M1 <$> runMessage1 msg x

instance (RunMessage1 env l, RunMessage1 env r) => RunMessage1 env (l :+: r) where
  runMessage1 msg (L1 x) = L1 <$> runMessage1 msg x
  runMessage1 msg (R1 x) = R1 <$> runMessage1 msg x

instance RunMessage env p => RunMessage1 env (K1 R p) where
  runMessage1 msg (K1 x) = K1 <$> runMessage msg x

class RunMessage env a where
  runMessage :: (MonadIO m, MonadRandom m, MonadReader env m, HasQueue env) => Message -> a -> m a
  default runMessage :: (Generic a, RunMessage1 env (Rep a), MonadIO m, MonadRandom m, MonadReader env m, HasQueue env) => Message -> a -> m a
  runMessage = defaultRunMessage

defaultRunMessage
  :: ( Generic a
     , RunMessage1 env (Rep a)
     , MonadIO m
     , MonadRandom m
     , MonadReader env m
     , HasQueue env
     )
  => Message
  -> a
  -> m a
defaultRunMessage msg = fmap to . runMessage1 msg . from

runMessageState :: Monad m => (s -> StateT s m a) -> s -> m s
runMessageState f s = execStateT (f s) s
