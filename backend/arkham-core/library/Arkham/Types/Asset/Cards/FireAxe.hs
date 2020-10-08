{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.FireAxe
  ( FireAxe(..)
  , fireAxe
  )
where

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

fightAbility :: AssetId -> Ability
fightAbility assetId =
  mkAbility (AssetSource assetId) 1 (ActionAbility 1 (Just Action.Fight))

reactionAbility :: AssetId -> SkillType -> Ability
reactionAbility assetId skillType =
  mkAbility (AssetSource assetId) 2 (ReactionAbility (WhenSkillTest skillType))

instance (HasSource ForSkillTest env, IsInvestigator investigator) => HasModifiersFor env investigator FireAxe where
  getModifiersFor SkillTestSource i (FireAxe a) | ownedBy a i = do
    using <- asks $ any (isSource a) . getSource ForSkillTest
    pure [ DamageDealt 1 | resourceCount i == 0 && using ]
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator FireAxe where
  getActions i NonFast (FireAxe a@Attrs {..}) | ownedBy a i = do
    fightAvailable <- hasFightActions i NonFast
    pure
      $ [ ActivateCardAbilityAction (getId () i) (fightAbility assetId)
        | fightAvailable && canDo Action.Fight i
        ]
  getActions i (WhenSkillTest skillType) (FireAxe a@Attrs {..}) | ownedBy a i =
    do
      let ability = reactionAbility assetId skillType
      using <- asks $ any (isSource a) . getSource ForSkillTest
      usedCount <-
        asks $ count (== (getId () i, ability)) . map unUsedAbility . getList ()
      pure
        [ ActivateCardAbilityAction (getId () i) ability
        | resourceCount i > 0 && using && usedCount < 3
        ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env FireAxe where
  runMessage msg a@(FireAxe attrs) = case msg of
    UseCardAbility iid _ source _ 1 | isSource attrs source ->
      a <$ unshiftMessage
        (ChooseFightEnemy
          iid
          source
          SkillCombat
          [SkillModifier SkillCombat 1]
          mempty
          False
        )
    UseCardAbility iid _ source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillCombat 2]
        ]
    _ -> FireAxe <$> runMessage msg attrs
