{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.FireAxe
  ( FireAxe(..)
  , fireAxe
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype FireAxe = FireAxe Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fireAxe :: AssetId -> FireAxe
fireAxe uuid = FireAxe $ (baseAttrs uuid "02032") { assetSlots = [HandSlot] }

fightAbility :: Attrs -> Ability
fightAbility Attrs { assetId } =
  mkAbility (AssetSource assetId) 1 (ActionAbility 1 (Just Action.Fight))

reactionAbility :: Attrs -> SkillType -> Ability
reactionAbility Attrs { assetId } skillType =
  mkAbility (AssetSource assetId) 2 (ReactionAbility (WhenSkillTest skillType))

instance HasCount ResourceCount env InvestigatorId => HasModifiersFor env FireAxe where
  getModifiersFor (SkillTestSource _ _ source (Just Action.Fight)) (InvestigatorTarget iid) (FireAxe a)
    | ownedBy a iid && isSource a source
    = do
      resourceCount <- getResourceCount iid
      pure [ DamageDealt 1 | resourceCount == 0 ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env FireAxe where
  getActions iid NonFast (FireAxe a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid NonFast
    pure $ [ ActivateCardAbilityAction iid (fightAbility a) | fightAvailable ]
  getActions iid (WhenSkillTest skillType) (FireAxe a) | ownedBy a iid = do
    let ability = reactionAbility a skillType
    msource <- asks $ getSource ForSkillTest
    let
      using = case msource of
        Just (SkillTestSource _ _ source (Just Action.Fight))
          | isSource a source -> True
        _ -> False
    usedCount <- count (== (iid, ability)) . map unUsedAbility <$> getList ()
    resourceCount <- getResourceCount iid
    pure
      [ ActivateCardAbilityAction iid ability
      | resourceCount > 0 && using && usedCount < 3
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env FireAxe where
  runMessage msg a@(FireAxe attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateSkillTestEffect
          (EffectModifiers [SkillModifier SkillCombat 1])
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat False
        ]
    UseCardAbility iid source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , CreateSkillTestEffect
          (EffectModifiers [SkillModifier SkillCombat 2])
          source
          (InvestigatorTarget iid)
        ]
    _ -> FireAxe <$> runMessage msg attrs
