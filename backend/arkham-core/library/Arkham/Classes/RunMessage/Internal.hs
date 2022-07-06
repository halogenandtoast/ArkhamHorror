module Arkham.Classes.RunMessage.Internal where

import Arkham.Message
import {-# SOURCE #-} Arkham.GameEnv

class RunMessage a where
  runMessage :: Message -> a -> GameT a
