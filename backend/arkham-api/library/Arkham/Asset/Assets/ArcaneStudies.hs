module Arkham.Asset.Assets.ArcaneStudies (arcaneStudies) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype ArcaneStudies = ArcaneStudies AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneStudies :: AssetCard ArcaneStudies
arcaneStudies = asset ArcaneStudies Cards.arcaneStudies

instance HasAbilities ArcaneStudies where
  getAbilities (ArcaneStudies a) =
    [ (cardI18n $ withI18nTooltip "arcaneStudies.fastSpend1ResourceYouGet1WillpowerForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #willpower)
        $ controlled a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , (cardI18n $ withI18nTooltip "arcaneStudies.fastSpend1ResourceYouGet1IntellectForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #intellect)
        $ controlled a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage ArcaneStudies where
  runMessage msg a@(ArcaneStudies attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (SkillModifier #willpower 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (SkillModifier #intellect 1)
      pure a
    _ -> ArcaneStudies <$> liftRunMessage msg attrs
