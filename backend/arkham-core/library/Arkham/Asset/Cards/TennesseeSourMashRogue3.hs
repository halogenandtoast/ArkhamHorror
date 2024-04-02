module Arkham.Asset.Cards.TennesseeSourMashRogue3 (tennesseeSourMashRogue3, TennesseeSourMashRogue3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude

newtype TennesseeSourMashRogue3 = TennesseeSourMashRogue3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tennesseeSourMashRogue3 :: AssetCard TennesseeSourMashRogue3
tennesseeSourMashRogue3 = asset TennesseeSourMashRogue3 Cards.tennesseeSourMashRogue3

instance HasAbilities TennesseeSourMashRogue3 where
  getAbilities (TennesseeSourMashRogue3 a) =
    [ controlledAbility a 1 (DuringSkillTest (SkillTestOnTreachery AnyTreachery))
        $ FastAbility (exhaust a <> assetUseCost a Supply 1)
    , restrictedAbility a 2 ControlsThis $ fightAction (discardCost a)
    ]

instance RunMessage TennesseeSourMashRogue3 where
  runMessage msg a@(TennesseeSourMashRogue3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ skillTestModifier attrs iid (SkillModifier #willpower 3)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifiers source iid [DamageDealt 1, SkillModifier #combat 3], chooseFight]
      pure a
    _ -> TennesseeSourMashRogue3 <$> runMessage msg attrs
