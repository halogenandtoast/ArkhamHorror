{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.DowntownArkhamAsylum where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype DowntownArkhamAsylum = DowntownArkhamAsylum Attrs
  deriving newtype (Show, ToJSON, FromJSON)

downtownArkhamAsylum :: DowntownArkhamAsylum
downtownArkhamAsylum =
  DowntownArkhamAsylum
    $ (baseAttrs "01131" "Downtown" 4 (PerPlayer 2) Triangle [Moon, T])
        { locationTraits = HashSet.fromList [Arkham]
        , locationVictory = Just 1
        }

instance HasActions DowntownArkhamAsylum where
  getActions (DowntownArkhamAsylum attrs) iid = getActions attrs iid

instance (LocationRunner env) => RunMessage env DowntownArkhamAsylum where
  runMessage msg (DowntownArkhamAsylum attrs) =
    DowntownArkhamAsylum <$> runMessage msg attrs
