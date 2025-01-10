module Arkham.Queue where

import Arkham.Classes.GameLogger
import Arkham.Prelude
import Control.Monad.Trans.Class
import Control.Monad.State.Strict

newtype Queue msg = Queue {queueToRef :: IORef [msg]}

newtype QueueT msg m a = QueueT {unQueueT :: ReaderT (Queue msg) m a}
  deriving newtype
    (Functor, Applicative, Monad, MonadIO, MonadRandom, MonadReader (Queue msg), MonadTrans)

instance HasGameLogger m => HasGameLogger (QueueT msg m) where
  getLogger = do
    logger <- lift getLogger
    pure $ \msg -> liftIO $ logger msg

instance MonadState s m => MonadState s (QueueT msg m) where
  get = lift get
  put = lift . put
