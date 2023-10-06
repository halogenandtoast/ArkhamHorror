module Arkham.Classes.RunMessage.Internal where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.GameLogger
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Message

class (HasGame m, HasQueue Message m, HasGameLogger m, MonadRandom m, CardGen m) => CanRun m

type Runner a = (forall m. (HasCallStack, CanRun m) => Message -> a -> m a)
type Runnable a = (forall m. (HasCallStack, CanRun m) => m a)

class RunMessage a where
  runMessage :: (HasCallStack, CanRun m) => Message -> a -> m a
