{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Easttown where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype Easttown = Easttown Attrs
  deriving newtype (Show, ToJSON, FromJSON)

easttown :: Easttown
easttown =
  Easttown
    $ (baseAttrs "01132" "Easttown" 2 (PerPlayer 1) Moon [Circle, Triangle])
        { locationTraits = HashSet.fromList [Arkham]
        }

instance (IsInvestigator investigator) => HasActions env investigator Easttown where
  getActions i window (Easttown attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Easttown where
  runMessage msg (Easttown attrs) = Easttown <$> runMessage msg attrs
