{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.JennysTwin45s where

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
import ClassyPrelude
import Lens.Micro

newtype JennysTwin45s = JennysTwin45s Attrs
  deriving newtype (Show, ToJSON, FromJSON)

jennysTwin45s :: AssetId -> JennysTwin45s
jennysTwin45s uuid =
  JennysTwin45s $ (baseAttrs uuid "02010") { assetSlots = [HandSlot, HandSlot] }

instance HasModifiersFor env investigator JennysTwin45s where
  getModifiersFor _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator JennysTwin45s where
  getActions i window (JennysTwin45s Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      fightAvailable <- hasFightActions i window
      pure
        [ ActivateCardAbilityAction
            (getId () i)
            (mkAbility
              (AssetSource assetId)
              1
              (ActionAbility 1 (Just Action.Fight))
            )
        | useCount assetUses > 0 && fightAvailable
        ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env JennysTwin45s where
  runMessage msg a@(JennysTwin45s attrs@Attrs {..}) = case msg of
    InvestigatorPlayDynamicAsset _ aid _ _ n | aid == assetId ->
      JennysTwin45s <$> runMessage msg (attrs & uses .~ Uses Resource.Ammo n)
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId ->
      case assetUses of
        Uses Resource.Ammo n -> do
          unshiftMessage
            (ChooseFightEnemy
              iid
              (AssetSource aid)
              SkillCombat
              [DamageDealt 1, SkillModifier SkillCombat 2]
              mempty
              False
            )
          pure $ JennysTwin45s $ attrs & uses .~ Uses Resource.Ammo (n - 1)
        _ -> pure a
    _ -> JennysTwin45s <$> runMessage msg attrs
