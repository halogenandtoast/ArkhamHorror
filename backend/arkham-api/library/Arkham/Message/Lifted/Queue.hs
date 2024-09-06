module Arkham.Message.Lifted.Queue where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Message
import Arkham.Prelude
import Arkham.Queue

class (CardGen m, HasGame m, HasQueue Message m) => ReverseQueue m
instance (CardGen m, MonadIO m, HasGame m) => ReverseQueue (QueueT Message m)
