module Arkham.Asset.Assets.SilverTongue (silverTongue) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestAction, isParley, withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype SilverTongue = SilverTongue AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTongue :: AssetCard SilverTongue
silverTongue = asset SilverTongue Cards.silverTongue

instance HasAbilities SilverTongue where
  getAbilities (SilverTongue a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {intellect} for this skill test. (+2 {intellect} instead if this is an evasion or parley)."
        $ wantsSkillTest (YourSkillTest #intellect)
        $ controlled a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {agility} for this skill test. (+2 {agility} instead if this is an evasion or parley)."
        $ wantsSkillTest (YourSkillTest #agility)
        $ controlled a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage SilverTongue where
  runMessage msg a@(SilverTongue attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        maction <- getSkillTestAction
        parley <- isParley
        let n =
              if maction == Just #evade || parley
                then 2
                else 1
        skillTestModifier sid attrs iid (SkillModifier #intellect n)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> do
        maction <- getSkillTestAction
        parley <- isParley
        let n =
              if maction == Just #evade || parley
                then 2
                else 1
        skillTestModifier sid attrs iid (SkillModifier #agility n)
      pure a
    _ -> SilverTongue <$> liftRunMessage msg attrs
