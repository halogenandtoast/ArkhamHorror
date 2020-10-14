{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ArkhamWoodsUnhallowedGround where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype ArkhamWoodsUnhallowedGround = ArkhamWoodsUnhallowedGround Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arkhamWoodsUnhallowedGround :: ArkhamWoodsUnhallowedGround
arkhamWoodsUnhallowedGround =
  ArkhamWoodsUnhallowedGround $ (baseAttrs
                                  "01150"
                                  "Arkham Woods: Unhallowed Ground"
                                  4
                                  (PerPlayer 1)
                                  Square
                                  [Squiggle]
                                  [Woods]
                                )
    { locationRevealedConnectedSymbols = HashSet.fromList
      [Squiggle, Hourglass, Diamond]
    , locationRevealedSymbol = Triangle
    }

instance HasModifiersFor env investigator ArkhamWoodsUnhallowedGround where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator ArkhamWoodsUnhallowedGround where
  getActions i window (ArkhamWoodsUnhallowedGround attrs) =
    getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsUnhallowedGround where
  runMessage msg (ArkhamWoodsUnhallowedGround attrs@Attrs {..}) = case msg of
    MoveTo iid lid | lid == locationId -> do
      unshiftMessage
        (BeginSkillTest
          iid
          (LocationSource "01150")
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          4
          []
          [InvestigatorAssignDamage iid (LocationSource "01150") 1 1]
          mempty
          mempty
        )
      ArkhamWoodsUnhallowedGround <$> runMessage msg attrs
    _ -> ArkhamWoodsUnhallowedGround <$> runMessage msg attrs
