module Arkham.Classes.RunMessage.Internal where

import Arkham.Prelude

import Arkham.Message
import {-# SOURCE #-} Arkham.GameEnv

class RunMessage a where
  runMessage :: HasCallStack => Message -> a -> GameT a
