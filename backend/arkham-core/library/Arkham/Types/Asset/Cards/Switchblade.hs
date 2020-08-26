{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Switchblade where

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
import ClassyPrelude

newtype Switchblade = Switchblade Attrs
  deriving newtype (Show, ToJSON, FromJSON)

switchblade :: AssetId -> Switchblade
switchblade uuid =
  Switchblade $ (baseAttrs uuid "01044") { assetSlots = [HandSlot] }

instance (ActionRunner env investigator) => HasActions env investigator Switchblade where
  getActions i window (Switchblade Attrs {..})
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
        | fightAvailable
        ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Switchblade where
  runMessage msg a@(Switchblade attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) 1 | aid == assetId ->
      a <$ unshiftMessage
        (ChooseFightEnemy
          iid
          SkillCombat
          [ModifierIfSucceededBy 2 (DamageDealt 1 (AssetSource assetId))]
          mempty
          False
        )
    _ -> Switchblade <$> runMessage msg attrs
