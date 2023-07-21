module Arkham.Game.Diff where

import Arkham.Prelude

import Arkham.Game.Base
import Arkham.Game.Json ()
import Data.Aeson
import Data.Aeson.Diff qualified as Diff

diff :: Game -> Game -> Diff.Patch
diff a b = Diff.diff (toJSON a) (toJSON b)

patch :: Game -> Diff.Patch -> Result Game
patch g p = case Diff.patch p (toJSON g) of
  Error e -> Error e
  Success a -> fromJSON a

unsafePatch :: Game -> Diff.Patch -> Game
unsafePatch g p = case patch g p of
  Success a -> a
  _ -> error "Could not patch safely"
