module Arkham.Asset.Assets.SpiritualIntuition3 (spiritualIntuition3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestSource, withSkillTest)
import Arkham.Helpers.Source (sourceMatches)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Ritual, Spell))

newtype SpiritualIntuition3 = SpiritualIntuition3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritualIntuition3 :: AssetCard SpiritualIntuition3
spiritualIntuition3 = asset SpiritualIntuition3 Cards.spiritualIntuition3

instance HasAbilities SpiritualIntuition3 where
  getAbilities (SpiritualIntuition3 a) =
    [ (cardI18n $ withI18nTooltip "spiritualIntuition3.fastSpend1ResourceYouGet1WillpowerForThisSkillTest2Willpower")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #willpower)
        $ controlled a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , (cardI18n $ withI18nTooltip "spiritualIntuition3.fastSpend1ResourceYouGet1CombatForThisSkillTest2CombatInstea")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #combat)
        $ controlled a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage SpiritualIntuition3 where
  runMessage msg a@(SpiritualIntuition3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        n <- runDefaultMaybeT 1 do
          source <- MaybeT getSkillTestSource
          liftGuardM $ sourceMatches source (mapOneOf SourceWithTrait [Spell, Ritual])
          pure 2
        skillTestModifier sid attrs iid (SkillModifier #willpower n)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> do
        n <- runDefaultMaybeT 1 do
          source <- MaybeT getSkillTestSource
          liftGuardM $ sourceMatches source $ mapOneOf SourceWithTrait [Spell, Ritual]
          pure 2
        skillTestModifier sid attrs iid (SkillModifier #combat n)
      pure a
    _ -> SpiritualIntuition3 <$> liftRunMessage msg attrs
