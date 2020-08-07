{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Flashlight where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..))
import qualified Arkham.Types.Asset.Uses as Resource
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude
import Lens.Micro

newtype FlashlightI = FlashlightI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

flashlight :: AssetId -> FlashlightI
flashlight uuid =
  FlashlightI $ (baseAttrs uuid "01087") { assetSlots = [HandSlot] }

instance (AssetRunner env) => RunMessage env FlashlightI where
  runMessage msg a@(FlashlightI attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      pure
        $ FlashlightI
        $ attrs
        & (uses .~ Uses Resource.Supply 3)
        & (abilities
          .~ [ ( AssetSource aid
               , Nothing
               , 1
               , ActionAbility 1 (Just Action.Investigate)
               , NoLimit
               )
             ]
          )
    UseCardAbility iid (AssetSource aid, _, 1, _, _) | aid == assetId ->
      case assetUses of
        Uses Resource.Supply n -> do
          when
            (n == 1)
            (unshiftMessage (RemoveAbilitiesFrom (AssetSource assetId)))
          lid <- asks (getId iid)
          unshiftMessages
            [ AddModifier
              (LocationTarget lid)
              (ShroudModifier (-2) (AssetSource aid))
            , Investigate iid lid SkillIntellect mempty False
            , RemoveAllModifiersOnTargetFrom
              (LocationTarget lid)
              (AssetSource aid)
            ]
          pure $ FlashlightI $ attrs & uses .~ Uses Resource.Supply (n - 1)
        _ -> pure a
    _ -> FlashlightI <$> runMessage msg attrs
