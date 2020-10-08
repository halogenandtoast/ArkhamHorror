{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.FireAxe where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window
import ClassyPrelude

newtype FireAxe = FireAxe Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fireAxe :: AssetId -> FireAxe
fireAxe uuid = FireAxe $ (baseAttrs uuid "02032") { assetSlots = [HandSlot] }

instance (HasSource ForSkillTest env, IsInvestigator investigator) => HasModifiersFor env investigator FireAxe where
  getModifiersFor SkillTestSource i (FireAxe Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      msource <- asks (getSource ForSkillTest)
      case msource of
        Just source' | source' == AssetSource assetId ->
          pure [ DamageDealt 1 | resourceCount i == 0 ]
        _ -> pure []
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator FireAxe where
  getActions i NonFast (FireAxe Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      fightAvailable <- hasFightActions i NonFast
      pure
        $ [ ActivateCardAbilityAction
              (getId () i)
              (mkAbility
                (AssetSource assetId)
                1
                (ActionAbility 1 (Just Action.Fight))
              )
          | fightAvailable && canDo Action.Fight i
          ]
  getActions i (WhenSkillTest skillType) (FireAxe Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      let
        ability = mkAbility
          (AssetSource assetId)
          2
          (ReactionAbility (WhenSkillTest skillType))
      testSource <- fromMaybe (error "missing source")
        <$> asks (getSource ForSkillTest)
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      let usedCount = count (== (getId () i, ability)) usedAbilities
      print usedCount
      pure
        [ ActivateCardAbilityAction (getId () i) ability
        | resourceCount i
          > 0
          && testSource
          == AssetSource assetId
          && usedCount
          < 3
        ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env FireAxe where
  runMessage msg a@(FireAxe attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId -> do
      unshiftMessage
        (ChooseFightEnemy
          iid
          (AssetSource aid)
          SkillCombat
          [SkillModifier SkillCombat 1]
          mempty
          False
        )
      pure a
    UseCardAbility iid _ (AssetSource aid) _ 2 | aid == assetId ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers
          SkillTestTarget
          (AssetSource aid)
          [SkillModifier SkillCombat 2]
        ]
    _ -> FireAxe <$> runMessage msg attrs
