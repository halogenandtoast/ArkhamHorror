module Arkham.Asset.Assets.Endurance (endurance) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest, getSkillTestAction)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype Endurance = Endurance AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endurance :: AssetCard Endurance
endurance = asset Endurance Cards.endurance

instance HasAbilities Endurance where
  getAbilities (Endurance a) =
    [ (cardI18n $ withI18nTooltip "endurance.fastSpend1Resource2")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #combat)
        $ controlled a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , (cardI18n $ withI18nTooltip "endurance.fastSpend1Resource")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #agility)
        $ controlled a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage Endurance where
  runMessage msg a@(Endurance attrs) = runQueueT $ case msg of
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
    _ -> Endurance <$> liftRunMessage msg attrs
