{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.FortyFiveAutomatic where

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

newtype FortyFiveAutomatic = FortyFiveAutomatic Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fortyFiveAutomatic :: AssetId -> FortyFiveAutomatic
fortyFiveAutomatic uuid =
  FortyFiveAutomatic $ (baseAttrs uuid "01016") { assetSlots = [HandSlot] }

instance HasModifiersFor env investigator FortyFiveAutomatic where
  getModifiersFor _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator FortyFiveAutomatic where
  getActions i window (FortyFiveAutomatic Attrs {..})
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

instance (AssetRunner env) => RunMessage env FortyFiveAutomatic where
  runMessage msg a@(FortyFiveAutomatic attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      FortyFiveAutomatic
        <$> runMessage msg (attrs & uses .~ Uses Resource.Ammo 4)
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId ->
      case assetUses of
        Uses Resource.Ammo n -> do
          unshiftMessage
            (ChooseFightEnemy
              iid
              (AssetSource aid)
              SkillCombat
              [DamageDealt 1, SkillModifier SkillCombat 1]
              mempty
              False
            )
          pure $ FortyFiveAutomatic $ attrs & uses .~ Uses Resource.Ammo (n - 1)
        _ -> pure a
    _ -> FortyFiveAutomatic <$> runMessage msg attrs
