module Arkham.Asset.Cards.Knife (
  Knife (..),
  knife,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype Knife = Knife AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knife :: AssetCard Knife
knife = asset Knife Cards.knife

instance HasAbilities Knife where
  getAbilities (Knife a) =
    [ withTooltip "{action}: _Fight_. You get +1 {combat} for this attack."
        $ restrictedAbility a 1 ControlsThis fightAction_
    , withTooltip
        "{action}: Discard Knife: _Fight_. You get +2 {combat} for this attack. This attack deals +1 damage."
        $ restrictedAbility a 2 ControlsThis
        $ fightAction (DiscardCost FromPlay (toTarget a))
    ]

instance RunMessage Knife where
  runMessage msg a@(Knife attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ skillTestModifier attrs iid (SkillModifier #combat 1)
        , chooseFightEnemy iid (toAbilitySource attrs 1) #combat
        ]
      pure a
    InDiscard _ (UseThisAbility iid (isSource attrs -> True) 2) -> do
      pushAll
        [ skillTestModifiers attrs iid [SkillModifier #combat 2, DamageDealt 1]
        , chooseFightEnemy iid (toAbilitySource attrs 2) #combat
        ]
      pure a
    _ -> Knife <$> runMessage msg attrs
