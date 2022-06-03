module Arkham.Classes.Depth where

import Arkham.Prelude

class HasDepth a where
  depthL :: Lens' a (IORef Int)
