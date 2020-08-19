{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Flashlight where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
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

newtype Flashlight = Flashlight Attrs
  deriving newtype (Show, ToJSON, FromJSON)

flashlight :: AssetId -> Flashlight
flashlight uuid =
  Flashlight $ (baseAttrs uuid "01087") { assetSlots = [HandSlot] }

instance (ActionRunner env investigator) => HasActions env investigator Flashlight where
  getActions i window (Flashlight Attrs {..}) = do
    investigateAvailable <- hasInvestigateActions i window
    pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility
            (AssetSource assetId)
            1
            (ActionAbility 1 (Just Action.Investigate))
          )
      | useCount assetUses > 0 && investigateAvailable
      ]

instance (AssetRunner env) => RunMessage env Flashlight where
  runMessage msg a@(Flashlight attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      pure $ Flashlight $ attrs & (uses .~ Uses Resource.Supply 3)
    UseCardAbility iid _ (AssetSource aid) 1 | aid == assetId ->
      case assetUses of
        Uses Resource.Supply n -> do
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
          pure $ Flashlight $ attrs & uses .~ Uses Resource.Supply (n - 1)
        _ -> pure a
    _ -> Flashlight <$> runMessage msg attrs
