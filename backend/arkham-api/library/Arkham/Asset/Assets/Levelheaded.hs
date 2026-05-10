module Arkham.Asset.Assets.Levelheaded (levelheaded) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getIsScenarioAbility, withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype Levelheaded = Levelheaded AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

levelheaded :: AssetCard Levelheaded
levelheaded = asset Levelheaded Cards.levelheaded

instance HasAbilities Levelheaded where
  getAbilities (Levelheaded a) =
    [ (cardI18n $ withI18nTooltip "levelheaded.fastSpend1ResourceYouGet1WillpowerForThisSkillTest2Willpower")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #willpower)
        $ controlled a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , (cardI18n $ withI18nTooltip "levelheaded.fastSpend1ResourceYouGet1AgilityForThisSkillTest2AgilityInst")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #agility)
        $ controlled a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage Levelheaded where
  runMessage msg a@(Levelheaded attrs) = runQueueT $ case msg of
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
    _ -> Levelheaded <$> liftRunMessage msg attrs
