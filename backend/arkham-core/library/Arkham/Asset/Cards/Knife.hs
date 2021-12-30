module Arkham.Asset.Cards.Knife
  ( Knife(..)
  , knife
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action (Action(Fight))
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target

newtype Knife = Knife AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knife :: AssetCard Knife
knife = asset Knife Cards.knife

instance HasAbilities Knife where
  getAbilities (Knife a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility (Just Fight) (ActionCost 1)
    , restrictedAbility a 2 OwnsThis
      $ ActionAbility
          (Just Fight)
          (Costs [ActionCost 1, DiscardCost (toTarget a)])
    ]

instance (AssetRunner env) => RunMessage env Knife where
  runMessage msg a@(Knife attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 1)
      , ChooseFightEnemy iid source Nothing SkillCombat mempty False
      ]
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifiers
        attrs
        (InvestigatorTarget iid)
        [SkillModifier SkillCombat 2, DamageDealt 1]
      , ChooseFightEnemy iid source Nothing SkillCombat mempty False
      ]
    _ -> Knife <$> runMessage msg attrs
