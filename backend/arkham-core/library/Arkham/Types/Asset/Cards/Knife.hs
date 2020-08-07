{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Knife where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import ClassyPrelude

newtype Knife = Knife Attrs
  deriving newtype (Show, ToJSON, FromJSON)

knife :: AssetId -> Knife
knife uuid = Knife $ (baseAttrs uuid "01086")
  { assetSlots = [HandSlot]
  , assetAbilities =
    [ ( AssetSource uuid
      , Nothing
      , 1
      , ActionAbility 1 (Just Action.Fight)
      , NoLimit
      )
    , ( AssetSource uuid
      , Nothing
      , 2
      , ActionAbility 1 (Just Action.Fight)
      , NoLimit
      )
    ]
  }

instance (AssetRunner env) => RunMessage env Knife where
  runMessage msg a@(Knife attrs@Attrs {..}) = case msg of
    UseCardAbility iid (AssetSource aid, _, 1, _, _) | aid == assetId -> do
      unshiftMessage
        (ChooseFightEnemy
          iid
          SkillCombat
          [SkillModifier SkillCombat 1 (AssetSource aid)]
          mempty
          False
        )
      pure a
    UseCardAbility iid (AssetSource aid, _, 2, _, _) | aid == assetId -> do
      unshiftMessages
        [ DiscardAsset aid
        , ChooseFightEnemy
          iid
          SkillCombat
          [ SkillModifier SkillCombat 2 (AssetSource aid)
          , DamageDealt 1 (AssetSource aid)
          ]
          mempty
          False
        ]
      pure a
    _ -> Knife <$> runMessage msg attrs
