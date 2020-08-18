{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Hallway where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import ClassyPrelude

newtype Hallway = Hallway Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hallway :: Hallway
hallway = Hallway
  $ baseAttrs "01112" "Hallway" 1 (Static 0) Square [Triangle, Plus, Diamond]

instance HasActions Hallway where
  getActions (Hallway attrs) iid = getActions attrs iid

instance (LocationRunner env) => RunMessage env Hallway where
  runMessage msg (Hallway attrs) = Hallway <$> runMessage msg attrs
