module Arkham.Classes.RunMessage where

import {-# SOURCE #-} Arkham.GameEnv
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Message

class RunMessage a where
  runMessage :: Message -> a -> GameT a
