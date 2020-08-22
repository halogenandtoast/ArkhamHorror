{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ArkhamWoodsQuietGlade where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype ArkhamWoodsQuietGlade = ArkhamWoodsQuietGlade Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arkhamWoodsQuietGlade :: ArkhamWoodsQuietGlade
arkhamWoodsQuietGlade = ArkhamWoodsQuietGlade $ (baseAttrs
                                                  "01155"
                                                  "Arkham Woods: Quiet Glade"
                                                  1
                                                  (Static 0)
                                                  Moon
                                                  [Squiggle, Equals, Hourglass]
                                                )
  { locationTraits = HashSet.fromList [Woods]
  }

instance (IsInvestigator investigator) => HasActions env investigator ArkhamWoodsQuietGlade where
  getActions i window (ArkhamWoodsQuietGlade attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsQuietGlade where
  runMessage msg (ArkhamWoodsQuietGlade attrs) =
    ArkhamWoodsQuietGlade <$> runMessage msg attrs
