{-# LANGUAGE DefaultSignatures #-}

module Arkham.Classes.RunMessage.Internal where

import Arkham.Classes.Entity
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
  default runMessage
    :: ( HasCallStack
       , Entity a
       , RunType a ~ a
       , RunType (EntityAttrs a) ~ EntityAttrs a
       , RunMessage (EntityAttrs a)
       )
    => Message -> a -> GameT (RunType a)
  runMessage msg = overAttrsM (runMessage msg)

liftRunMessage
  :: (HasCallStack, MonadTrans t, RunMessage a, a ~ RunType a) => Message -> a -> t GameT a
liftRunMessage msg = lift . runMessage msg
