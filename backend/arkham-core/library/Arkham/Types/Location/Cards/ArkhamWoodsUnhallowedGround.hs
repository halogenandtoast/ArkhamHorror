{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ArkhamWoodsUnhallowedGround where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ArkhamWoodsUnhallowedGround = ArkhamWoodsUnhallowedGround Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arkhamWoodsUnhallowedGround :: ArkhamWoodsUnhallowedGround
arkhamWoodsUnhallowedGround = ArkhamWoodsUnhallowedGround $ base
  { locationRevealedConnectedSymbols = setFromList
    [Squiggle, Hourglass, Diamond]
  , locationRevealedSymbol = Triangle
  }
 where
  base = baseAttrs
    "01150"
    "Arkham Woods: Unhallowed Ground"
    EncounterSet.TheDevourerBelow
    4
    (PerPlayer 1)
    Square
    [Squiggle]
    [Woods]

instance HasModifiersFor env ArkhamWoodsUnhallowedGround where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ArkhamWoodsUnhallowedGround where
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
