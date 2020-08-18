{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Northside where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype Northside = Northside Attrs
  deriving newtype (Show, ToJSON, FromJSON)

northside :: Northside
northside =
  Northside
    $ (baseAttrs "01134" "Northside" 3 (PerPlayer 2) T [Diamond, Triangle])
        { locationTraits = HashSet.fromList [Arkham]
        , locationVictory = Just 1
        }

instance HasActions Northside where
  getActions (Northside attrs) iid = getActions attrs iid

instance (LocationRunner env) => RunMessage env Northside where
  runMessage msg (Northside attrs) = Northside <$> runMessage msg attrs
