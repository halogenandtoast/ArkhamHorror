{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Northside where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
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

instance (CanInvestigate LocationId investigator, HasId InvestigatorId () investigator) => HasActions investigator Northside where
  getActions i (Northside attrs) = getActions i attrs

instance (LocationRunner env) => RunMessage env Northside where
  runMessage msg (Northside attrs) = Northside <$> runMessage msg attrs
