module Arkham.Asset.Cards.MonstrousTransformation
  ( MonstrousTransformation(..)
  , monstrousTransformation
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.SkillType

newtype MonstrousTransformation = MonstrousTransformation AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monstrousTransformation :: AssetCard MonstrousTransformation
monstrousTransformation = assetWith
  MonstrousTransformation
  Cards.monstrousTransformation
  (isStoryL .~ True)

instance HasModifiersFor MonstrousTransformation where
  getModifiersFor (InvestigatorTarget iid) (MonstrousTransformation a)
    | controlledBy a iid = pure $ toModifiers
      a
      [ BaseSkillOf SkillWillpower 2
      , BaseSkillOf SkillIntellect 2
      , BaseSkillOf SkillCombat 5
      , BaseSkillOf SkillAgility 5
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities MonstrousTransformation where
  getAbilities (MonstrousTransformation a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility
        (Just Action.Fight)
        (Costs [ExhaustCost (toTarget a), ActionCost 1])
    ]

instance RunMessage MonstrousTransformation where
  runMessage msg a@(MonstrousTransformation attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1)
      , ChooseFightEnemy iid source Nothing SkillCombat mempty False
      ]
    _ -> MonstrousTransformation <$> runMessage msg attrs
