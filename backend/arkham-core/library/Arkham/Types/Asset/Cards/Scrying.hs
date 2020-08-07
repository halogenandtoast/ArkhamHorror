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

newtype Scrying = Scrying Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

scrying :: AssetId -> Scrying
scrying uuid =
  Scrying $ (baseAttrs uuid "01061") { assetSlots = [ArcaneSlot] }

instance (AssetRunner env) => RunMessage env Scrying where
  runMessage msg a@(Scrying attrs@Attrs {..}) = case msg of
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
      Scrying <$> runMessage msg attrs'
    UseCardAbility iid (AssetSource aid, _, 1, _, _) | aid == assetId ->
      case assetUses of
        Uses Resource.Charge n -> do
          when
            (n == 1)
            (unshiftMessage (RemoveAbilitiesFrom (AssetSource assetId)))
          unshiftMessage (SearchTopOfDeck iid 3 [] PutBackInAnyOrder)
          pure $ Scrying $ attrs & uses .~ Uses Resource.Charge (n - 1)
        _ -> pure a
    _ -> Scrying <$> runMessage msg attrs
