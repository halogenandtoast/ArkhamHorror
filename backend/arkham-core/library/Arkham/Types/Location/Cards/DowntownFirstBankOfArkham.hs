{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.DowntownFirstBankOfArkham where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype DowntownFirstBankOfArkham = DowntownFirstBankOfArkham Attrs
  deriving newtype (Show, ToJSON, FromJSON)

downtownFirstBankOfArkham :: DowntownFirstBankOfArkham
downtownFirstBankOfArkham =
  DowntownFirstBankOfArkham
    $ (baseAttrs "01130" "Downtown" 3 (PerPlayer 1) Triangle [Moon, T])
        { locationTraits = HashSet.fromList [Arkham]
        }

instance HasActions DowntownFirstBankOfArkham where
  getActions (DowntownFirstBankOfArkham attrs) iid = getActions attrs iid

instance (LocationRunner env) => RunMessage env DowntownFirstBankOfArkham where
  runMessage msg (DowntownFirstBankOfArkham attrs) =
    DowntownFirstBankOfArkham <$> runMessage msg attrs
