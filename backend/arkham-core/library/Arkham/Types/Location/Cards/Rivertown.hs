{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Rivertown where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype Rivertown = Rivertown Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rivertown :: Rivertown
rivertown = Rivertown $ (baseAttrs
                          "01125"
                          "Rivertown"
                          1
                          (PerPlayer 1)
                          Circle
                          [Moon, Diamond, Square, Squiggle, Hourglass]
                        )
  { locationTraits = HashSet.fromList [Arkham, Central]
  }

instance HasActions Rivertown where
  getActions (Rivertown attrs) iid = getActions attrs iid

instance (LocationRunner env) => RunMessage env Rivertown where
  runMessage msg (Rivertown attrs) = Rivertown <$> runMessage msg attrs
