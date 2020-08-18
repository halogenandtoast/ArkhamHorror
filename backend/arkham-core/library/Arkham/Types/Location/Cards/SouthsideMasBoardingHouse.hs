{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.SouthsideMasBoardingHouse where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype SouthsideMasBoardingHouse = SouthsideMasBoardingHouse Attrs
  deriving newtype (Show, ToJSON, FromJSON)

southsideMasBoardingHouse :: SouthsideMasBoardingHouse
southsideMasBoardingHouse =
  SouthsideMasBoardingHouse
    $ (baseAttrs
        "01127"
        "Southside"
        2
        (PerPlayer 1)
        Square
        [Diamond, Plus, Circle]
      )
        { locationTraits = HashSet.fromList [Arkham]
        }

instance HasActions SouthsideMasBoardingHouse where
  getActions (SouthsideMasBoardingHouse attrs) iid = getActions attrs iid

instance (LocationRunner env) => RunMessage env SouthsideMasBoardingHouse where
  runMessage msg (SouthsideMasBoardingHouse attrs) =
    SouthsideMasBoardingHouse <$> runMessage msg attrs
