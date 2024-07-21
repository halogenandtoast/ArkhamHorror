module Arkham.Asset.Cards.Knife (Knife (..), knife) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

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
        $ fightAction (discardCost a)
    ]

instance RunMessage Knife where
  runMessage msg a@(Knife attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll [skillTestModifier sid source iid (SkillModifier #combat 1), chooseFight]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll [skillTestModifiers sid source iid [SkillModifier #combat 2, DamageDealt 1], chooseFight]
      pure a
    _ -> Knife <$> runMessage msg attrs
