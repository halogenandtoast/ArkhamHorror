{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Shotgun4 where

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

newtype Shotgun4 = Shotgun4 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

shotgun4 :: AssetId -> Shotgun4
shotgun4 uuid =
  Shotgun4 $ (baseAttrs uuid "01029") { assetSlots = [HandSlot, HandSlot] }

instance HasModifiersFor env investigator Shotgun4 where
  getModifiersFor _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator Shotgun4 where
  getActions i window (Shotgun4 Attrs {..})
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

instance (AssetRunner env) => RunMessage env Shotgun4 where
  runMessage msg a@(Shotgun4 attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      Shotgun4 <$> runMessage msg (attrs & uses .~ Uses Resource.Ammo 2)
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId ->
      case assetUses of
        Uses Resource.Ammo n -> do
          unshiftMessage
            (ChooseFightEnemy
              iid
              SkillCombat
              [SkillModifier SkillCombat 3]
              mempty
              False
            )
          pure $ Shotgun4 $ attrs & uses .~ Uses Resource.Ammo (n - 1)
        _ -> pure a
    FailedSkillTest _ _ (AssetSource aid) SkillTestInitiatorTarget n
      | aid == assetId
      -> let val = min 1 (max 5 n)
         in
           a <$ unshiftMessage
             (AddModifiers SkillTestTarget (AssetSource aid) [DamageDealt val])
    PassedSkillTest _ _ (AssetSource aid) SkillTestInitiatorTarget n
      | aid == assetId
      -> let val = min 1 (max 5 n)
         in
           a <$ unshiftMessage
             (AddModifiers SkillTestTarget (AssetSource aid) [DamageDealt val])
    _ -> Shotgun4 <$> runMessage msg attrs
