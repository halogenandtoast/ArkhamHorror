module Arkham.Asset.Assets.SpiritualIntuition (spiritualIntuition) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestSource, withSkillTest)
import Arkham.Helpers.Source (sourceMatches)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Ritual, Spell))

newtype SpiritualIntuition = SpiritualIntuition AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritualIntuition :: AssetCard SpiritualIntuition
spiritualIntuition = asset SpiritualIntuition Cards.spiritualIntuition

instance HasAbilities SpiritualIntuition where
  getAbilities (SpiritualIntuition a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {willpower} for this skill test. (+2 {willpower} instead if this test is on a a _Spell_ or _Ritual_ card)."
        $ wantsSkillTest (YourSkillTest #willpower)
        $ controlled a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {combat} for this skill test. (+2 {combat} instead if this test is on a _Spell_ or _Ritual_ card)."
        $ wantsSkillTest (YourSkillTest #combat)
        $ controlled a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage SpiritualIntuition where
  runMessage msg a@(SpiritualIntuition attrs) = runQueueT $ case msg of
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
    _ -> SpiritualIntuition <$> liftRunMessage msg attrs
