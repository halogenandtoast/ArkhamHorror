module Arkham.Asset.Cards.SteadyHanded1 (steadyHanded1, steadyHanded1Effect, SteadyHanded1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.Investigator (canHaveHorrorHealed)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher hiding (SkillTestEnded)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype SteadyHanded1 = SteadyHanded1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

steadyHanded1 :: AssetCard SteadyHanded1
steadyHanded1 = asset SteadyHanded1 Cards.steadyHanded1

instance HasAbilities SteadyHanded1 where
  getAbilities (SteadyHanded1 x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility (WouldHaveSkillTestResult #when You #any #success) (exhaust x)
    ]

instance RunMessage SteadyHanded1 where
  runMessage msg a@(SteadyHanded1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        let modify = skillTestModifier sid (attrs.ability 1) sid . SkillTestResultValueModifier
        createCardEffect Cards.steadyHanded1 (effectMetaTarget sid) (attrs.ability 1) iid
        chooseOrRunOneM iid do
          labeled "Succeed by 1 less" do
            modify (-1)
            push RecalculateSkillTestResults
          labeled "Succeed by 1 more" do
            modify 1
            push RecalculateSkillTestResults
      pure a
    _ -> SteadyHanded1 <$> liftRunMessage msg attrs

newtype SteadyHanded1Effect = SteadyHanded1Effect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

steadyHanded1Effect :: EffectArgs -> SteadyHanded1Effect
steadyHanded1Effect = cardEffect SteadyHanded1Effect Cards.steadyHanded1

instance RunMessage SteadyHanded1Effect where
  runMessage msg e@(SteadyHanded1Effect attrs) = runQueueT $ case msg of
    SkillTestEnded sid | Just sid == attrs.skillTest -> disableReturn e
    PassedSkillTest _ _ _ _ _ n -> do
      withSkillTest \sid -> do
        when (Just sid == attrs.skillTest && n == 2) do
          case attrs.target of
            InvestigatorTarget iid -> do
              canHeal <- canHaveHorrorHealed attrs.source iid
              when canHeal do
                push $ HealHorror (toTarget iid) attrs.source 1
                disable attrs
            _ -> pure ()
      pure e
    _ -> SteadyHanded1Effect <$> liftRunMessage msg attrs
