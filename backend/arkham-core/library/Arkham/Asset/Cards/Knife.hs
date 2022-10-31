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
import Arkham.SkillType
import Arkham.Target

newtype Knife = Knife AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knife :: AssetCard Knife
knife = asset Knife Cards.knife

instance HasAbilities Knife where
  getAbilities (Knife a) =
    [ withTooltip "{action}: _Fight_. You get +1 {combat} for this attack."
      $ restrictedAbility a 1 ControlsThis $ ActionAbility (Just Fight) (ActionCost 1)
    , withTooltip "{action}: Discard Knife: _Fight_. You get +2 {combat} for this attack. This attack deals +1 damage."
      $ restrictedAbility a 2 ControlsThis
      $ ActionAbility
          (Just Fight)
          (Costs [ActionCost 1, DiscardCost (toTarget a)])
    ]

instance RunMessage Knife where
  runMessage msg a@(Knife attrs) = case msg of
    UseCardAbility iid (isAbility attrs 1 -> True) _ _ -> a <$ pushAll
      [ skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 1)
      , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
      ]
    InDiscard _ (UseCardAbility iid (isAbility attrs 2 -> True) _ _) -> a <$ pushAll
      [ skillTestModifiers
        attrs
        (InvestigatorTarget iid)
        [SkillModifier SkillCombat 2, DamageDealt 1]
      , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
      ]
    _ -> Knife <$> runMessage msg attrs
