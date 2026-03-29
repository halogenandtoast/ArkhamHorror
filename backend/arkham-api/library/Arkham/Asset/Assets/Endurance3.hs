module Arkham.Asset.Assets.Endurance3 (endurance3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest, getSkillTestAction)
import Arkham.Matcher
import Arkham.Modifier

newtype Endurance3 = Endurance3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endurance3 :: AssetCard Endurance3
endurance3 = asset Endurance3 Cards.endurance3

instance HasAbilities Endurance3 where
  getAbilities (Endurance3 a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {combat} for this skill test. (+2 {combat} instead if this is an attack or evasion)."
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #combat)
        $ controlled a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {agility} for this skill test. (+2 {agility} instead if this is an attack or evasion)."
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #agility)
        $ controlled a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage Endurance3 where
  runMessage msg a@(Endurance3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        maction <- getSkillTestAction
        let n = case maction of
              Just action | action `elem` [#fight, #evade] -> 2
              _ -> 1
        skillTestModifier sid attrs iid (SkillModifier #combat n)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> do
        maction <- getSkillTestAction
        let n = case maction of
              Just action | action `elem` [#fight, #evade] -> 2
              _ -> 1
        skillTestModifier sid attrs iid (SkillModifier #agility n)
      pure a
    _ -> Endurance3 <$> liftRunMessage msg attrs
