module Arkham.Asset.Assets.SilverTongue3 (silverTongue3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestAction, isParley, withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype SilverTongue3 = SilverTongue3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTongue3 :: AssetCard SilverTongue3
silverTongue3 = asset SilverTongue3 Cards.silverTongue3

instance HasAbilities SilverTongue3 where
  getAbilities (SilverTongue3 a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {intellect} for this skill test. (+2 {intellect} instead if this is an evasion or parley)."
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #intellect)
        $ controlled a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {agility} for this skill test. (+2 {agility} instead if this is an evasion or parley)."
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #agility)
        $ controlled a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage SilverTongue3 where
  runMessage msg a@(SilverTongue3 attrs) = runQueueT $ case msg of
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
    _ -> SilverTongue3 <$> liftRunMessage msg attrs
