{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.YourHouse where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype YourHouse = YourHouse Attrs
  deriving newtype (Show, ToJSON, FromJSON)

yourHouse :: YourHouse
yourHouse =
  YourHouse $ (baseAttrs "01124" "Your House" 2 (PerPlayer 1) Squiggle [Circle])
    { locationTraits = HashSet.fromList [Arkham]
    }

instance HasActions YourHouse where
  getActions (YourHouse attrs) iid = getActions attrs iid

instance (LocationRunner env) => RunMessage env YourHouse where
  runMessage msg (YourHouse attrs) = YourHouse <$> runMessage msg attrs
