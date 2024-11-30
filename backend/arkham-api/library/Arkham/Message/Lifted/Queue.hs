module Arkham.Message.Lifted.Queue where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Id
import Arkham.Message
import Arkham.Prelude
import Arkham.Queue

class (IdGen m, CardGen m, HasGame m, HasQueue Message m) => ReverseQueue m
instance (IdGen m, CardGen m, MonadIO m, HasGame m) => ReverseQueue (QueueT Message m)
