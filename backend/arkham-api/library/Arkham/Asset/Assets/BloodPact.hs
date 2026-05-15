module Arkham.Asset.Assets.BloodPact (bloodPact) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Prelude

newtype BloodPact = BloodPact AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodPact :: AssetCard BloodPact
bloodPact = asset BloodPact Cards.bloodPact

instance HasAbilities BloodPact where
  getAbilities (BloodPact x) =
    [ (cardI18n $ withI18nTooltip "bloodPact.fastAdd1Doom2")
        $ playerLimit PerTestOrAbility
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #willpower)
        $ controlled x 1 DuringAnySkillTest (FastAbility $ DoomCost (x.ability 1) (toTarget x) 1)
    , (cardI18n $ withI18nTooltip "bloodPact.fastAdd1Doom")
        $ playerLimit PerTestOrAbility
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #combat)
        $ controlled x 2 DuringAnySkillTest (FastAbility $ DoomCost (x.ability 2) (toTarget x) 1)
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
