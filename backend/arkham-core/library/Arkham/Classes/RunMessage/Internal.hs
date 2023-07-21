module Arkham.Classes.RunMessage.Internal where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Message

type Runner a = Message -> a -> GameT a

class RunMessage a where
  runMessage :: HasCallStack => Message -> a -> GameT a
