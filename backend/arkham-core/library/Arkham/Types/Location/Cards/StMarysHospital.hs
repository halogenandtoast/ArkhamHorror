{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.StMarysHospital where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype StMarysHospital = StMarysHospital Attrs
  deriving newtype (Show, ToJSON, FromJSON)

stMarysHospital :: StMarysHospital
stMarysHospital =
  StMarysHospital
    $ (baseAttrs
        "01128"
        "St. Mary's Hospital"
        2
        (PerPlayer 1)
        Plus
        [Diamond, Square]
      )
        { locationTraits = HashSet.fromList [Arkham]
        }

instance HasActions StMarysHospital where
  getActions (StMarysHospital attrs) iid = getActions attrs iid

instance (LocationRunner env) => RunMessage env StMarysHospital where
  runMessage msg (StMarysHospital attrs) =
    StMarysHospital <$> runMessage msg attrs
