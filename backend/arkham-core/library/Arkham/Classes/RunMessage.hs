module Arkham.Classes.RunMessage where

import Arkham.GameEnv
import Arkham.Message

class RunMessage a where
  runMessage :: Message -> a -> GameT a
