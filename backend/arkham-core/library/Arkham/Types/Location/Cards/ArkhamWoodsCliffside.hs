{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ArkhamWoodsCliffside where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype ArkhamWoodsCliffside = ArkhamWoodsCliffside Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arkhamWoodsCliffside :: ArkhamWoodsCliffside
arkhamWoodsCliffside = ArkhamWoodsCliffside $ (baseAttrs
                                                "01153"
                                                "Arkham Woods: Cliffside"
                                                2
                                                (PerPlayer 1)
                                                Square
                                                [Squiggle]
                                                [Woods]
                                              )
  { locationRevealedConnectedSymbols = HashSet.fromList
    [Squiggle, Moon, Triangle]
  , locationRevealedSymbol = Hourglass
  }

instance (IsInvestigator investigator) => HasActions env investigator ArkhamWoodsCliffside where
  getActions i window (ArkhamWoodsCliffside attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsCliffside where
  runMessage msg (ArkhamWoodsCliffside attrs@Attrs {..}) = case msg of
    Investigate iid lid source _ modifiers' tokenResponses overrides False
      | lid == locationId
      -> ArkhamWoodsCliffside
        <$> runMessage
              (Investigate
                iid
                lid
                source
                SkillAgility
                modifiers'
                tokenResponses
                overrides
                False
              )
              attrs
    _ -> ArkhamWoodsCliffside <$> runMessage msg attrs
