{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.SouthsideHistoricalSociety where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype SouthsideHistoricalSociety = SouthsideHistoricalSociety Attrs
  deriving newtype (Show, ToJSON, FromJSON)

southsideHistoricalSociety :: SouthsideHistoricalSociety
southsideHistoricalSociety =
  SouthsideHistoricalSociety
    $ (baseAttrs
        "01126"
        "Southside"
        3
        (PerPlayer 1)
        Square
        [Diamond, Plus, Circle]
      )
        { locationTraits = HashSet.fromList [Arkham]
        }

instance HasActions SouthsideHistoricalSociety where
  getActions (SouthsideHistoricalSociety attrs) iid = getActions attrs iid

instance (LocationRunner env) => RunMessage env SouthsideHistoricalSociety where
  runMessage msg (SouthsideHistoricalSociety attrs) =
    SouthsideHistoricalSociety <$> runMessage msg attrs
