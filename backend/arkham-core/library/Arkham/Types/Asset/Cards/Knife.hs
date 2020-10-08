{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Knife where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window
import ClassyPrelude

newtype Knife = Knife Attrs
  deriving newtype (Show, ToJSON, FromJSON)

knife :: AssetId -> Knife
knife uuid = Knife $ (baseAttrs uuid "01086") { assetSlots = [HandSlot] }

instance HasModifiersFor env investigator Knife where
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator Knife where
  getActions i NonFast (Knife Attrs {..})
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
        <> [ ActivateCardAbilityAction
               (getId () i)
               (mkAbility
                 (AssetSource assetId)
                 2
                 (ActionAbility 1 (Just Action.Fight))
               )
           | fightAvailable && canDo Action.Fight i
           ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Knife where
  runMessage msg a@(Knife attrs@Attrs {..}) = case msg of
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
    UseCardAbility iid _ (AssetSource aid) _ 2 | aid == assetId -> do
      unshiftMessages
        [ Discard (AssetTarget aid)
        , ChooseFightEnemy
          iid
          (AssetSource aid)
          SkillCombat
          [SkillModifier SkillCombat 2, DamageDealt 1]
          mempty
          False
        ]
      pure a
    _ -> Knife <$> runMessage msg attrs
