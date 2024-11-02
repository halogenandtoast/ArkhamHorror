module Arkham.Asset.Assets.BloodPact (bloodPact, BloodPact (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype BloodPact = BloodPact AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodPact :: AssetCard BloodPact
bloodPact = asset BloodPact Cards.bloodPact

instance HasAbilities BloodPact where
  getAbilities (BloodPact x) =
    [ withTooltip
        "{fast} Add 1 doom to Blood Pact: You get +2 {willpower} for this skill test. (Limit once per test.)"
        $ playerLimit PerTestOrAbility
        $ wantsSkillTest (YourSkillTest #willpower)
        $ restrictedAbility x 1 ControlsThis (FastAbility $ DoomCost (x.ability 1) (toTarget x) 1)
    , withTooltip
        "{fast} Add 1 doom to Blood Pact: You get +2 {combat} for this skill test. (Limit once per test.)"
        $ playerLimit PerTestOrAbility
        $ wantsSkillTest (YourSkillTest #combat)
        $ restrictedAbility x 2 ControlsThis (FastAbility $ DoomCost (x.ability 2) (toTarget x) 2)
    ]

instance RunMessage BloodPact where
  runMessage msg a@(BloodPact attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        pushM $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        pushM $ skillTestModifier sid (attrs.ability 2) iid (SkillModifier #combat 2)
      pure a
    _ -> BloodPact <$> runMessage msg attrs
