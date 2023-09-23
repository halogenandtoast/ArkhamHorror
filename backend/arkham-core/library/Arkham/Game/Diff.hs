module Arkham.Game.Diff where

import Arkham.Prelude

import Arkham.Game.Base
import Arkham.Game.Json ()
import Data.Aeson
import Data.Aeson.Diff qualified as Diff

-- We need to exclude gameActionDiff since it will cause a very large diff
-- We will just copy it directly
diff :: Game -> Game -> Diff.Patch
diff a b = Diff.diff (toJSON (a {gameActionDiff = []})) (toJSON (b {gameActionDiff = []}))

patch :: Game -> Diff.Patch -> Result Game
patch g p = case Diff.patch p (toJSON g) of
  Error e -> Error e
  Success a -> fromJSON a

unsafePatch :: Game -> Diff.Patch -> Game
unsafePatch g p = case patch g p of
  Success a -> a
  _ -> error "Could not patch safely"
