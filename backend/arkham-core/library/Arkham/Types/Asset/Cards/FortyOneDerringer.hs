{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.FortyOneDerringer where

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

newtype FortyOneDerringer = FortyOneDerringer Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fortyOneDerringer :: AssetId -> FortyOneDerringer
fortyOneDerringer uuid =
  FortyOneDerringer $ (baseAttrs uuid "01047") { assetSlots = [HandSlot] }

instance HasModifiersFor env investigator FortyOneDerringer where
  getModifiersFor _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator FortyOneDerringer where
  getActions i window (FortyOneDerringer Attrs {..})
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

instance (AssetRunner env) => RunMessage env FortyOneDerringer where
  runMessage msg a@(FortyOneDerringer attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      FortyOneDerringer
        <$> runMessage msg (attrs & uses .~ Uses Resource.Ammo 3)
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId ->
      case assetUses of
        Uses Resource.Ammo n -> do
          unshiftMessage
            (ChooseFightEnemy
              iid
              SkillCombat
              [ ModifierIfSucceededBy 2 (DamageDealt 1)
              , SkillModifier SkillCombat 2
              ]
              mempty
              False
            )
          pure $ FortyOneDerringer $ attrs & uses .~ Uses Resource.Ammo (n - 1)
        _ -> pure a
    _ -> FortyOneDerringer <$> runMessage msg attrs
