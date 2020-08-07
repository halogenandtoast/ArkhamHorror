{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Scrying where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..))
import qualified Arkham.Types.Asset.Uses as Resource
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import ClassyPrelude
import Lens.Micro

newtype ScryingI = ScryingI Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

scrying :: AssetId -> ScryingI
scrying uuid =
  ScryingI $ (baseAttrs uuid "01061") { assetSlots = [ArcaneSlot] }

instance (AssetRunner env) => RunMessage env ScryingI where
  runMessage msg a@(ScryingI attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId -> do
      let
        attrs' =
          attrs
            & (uses .~ Uses Resource.Charge 3)
            & (abilities
              .~ [ ( AssetSource aid
                   , Nothing
                   , 1
                   , ActionAbility 1 Nothing
                   , NoLimit
                   )
                 ]
              )
      ScryingI <$> runMessage msg attrs'
    UseCardAbility iid (AssetSource aid, _, 1, _, _) | aid == assetId ->
      case assetUses of
        Uses Resource.Charge n -> do
          when
            (n == 1)
            (unshiftMessage (RemoveAbilitiesFrom (AssetSource assetId)))
          unshiftMessage (SearchTopOfDeck iid 3 [] PutBackInAnyOrder)
          pure $ ScryingI $ attrs & uses .~ Uses Resource.Charge (n - 1)
        _ -> pure a
    _ -> ScryingI <$> runMessage msg attrs
