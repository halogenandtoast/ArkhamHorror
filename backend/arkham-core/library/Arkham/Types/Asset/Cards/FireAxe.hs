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
import Arkham.Types.Helpers

newtype FireAxe = FireAxe Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fireAxe :: AssetId -> FireAxe
fireAxe uuid = FireAxe $ baseAttrs uuid "02032" $ slots .= [HandSlot]

fightAbility :: Attrs -> Ability
fightAbility Attrs { assetId } =
  mkAbility (AssetSource assetId) 1 (ActionAbility 1 (Just Action.Fight))

reactionAbility :: Attrs -> SkillType -> Ability
reactionAbility Attrs { assetId } skillType =
  mkAbility (AssetSource assetId) 2 (ReactionAbility (WhenSkillTest skillType))

instance AssetRunner env => HasModifiersFor env FireAxe where
  getModifiersFor (SkillTestSource _ source (Just Action.Fight)) (InvestigatorTarget iid) (FireAxe a)
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
        Just (SkillTestSource _ source (Just Action.Fight))
          | isSource a source -> True
        _ -> False
    usedCount <-
      asks $ count (== (iid, ability)) . map unUsedAbility . getList ()
    resourceCount <- getResourceCount iid
    pure
      [ ActivateCardAbilityAction iid ability
      | resourceCount > 0 && using && usedCount < 3
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env FireAxe where
  runMessage msg a@(FireAxe attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> a <$ unshiftMessage
      (ChooseFightEnemy
        iid
        source
        SkillCombat
        [SkillModifier SkillCombat 1]
        mempty
        False
      )
    UseCardAbility iid source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillCombat 2]
        ]
    _ -> FireAxe <$> runMessage msg attrs
