module Arkham.Classes.RunMessage.Internal where

import Arkham.GameT
import {-# SOURCE #-} Arkham.Message
import Arkham.Prelude
import Control.Monad.Trans

type Runner a = HasCallStack => Message -> a -> GameT a
type Runnable a = HasCallStack => GameT a

class RunMessage a where
  type RunType a :: Type
  type RunType a = a
  runMessage :: HasCallStack => Message -> a -> GameT (RunType a)

liftRunMessage :: (HasCallStack, MonadTrans t, RunMessage a, a ~ RunType a) => Message -> a -> t GameT a
liftRunMessage msg = lift . runMessage msg
