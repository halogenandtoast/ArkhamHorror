module Arkham.Asset.Assets.Levelheaded3 (levelheaded3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getIsScenarioAbility, withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype Levelheaded3 = Levelheaded3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

levelheaded3 :: AssetCard Levelheaded3
levelheaded3 = asset Levelheaded3 Cards.levelheaded3

instance HasAbilities Levelheaded3 where
  getAbilities (Levelheaded3 a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {willpower} for this skill test. (+2 {willpower} instead if this test is on a scenario card)."
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #willpower)
        $ controlled a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {agility} for this skill test. (+2 {agility} instead if this test is on a scenario card)."
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #agility)
        $ controlled a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage Levelheaded3 where
  runMessage msg a@(Levelheaded3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        isScenario <- getIsScenarioAbility
        let n = if isScenario then 2 else 1
        skillTestModifier sid attrs iid (SkillModifier #willpower n)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> do
        isScenario <- getIsScenarioAbility
        let n = if isScenario then 2 else 1
        skillTestModifier sid attrs iid (SkillModifier #agility n)
      pure a
    _ -> Levelheaded3 <$> liftRunMessage msg attrs
