module Arkham.Asset.Assets.BreathOfTheSleeper (breathOfTheSleeper) where

import Arkham.Ability
import Arkham.Aspect.Types
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosBagStepState
import Arkham.ChaosToken.Types
import Arkham.Fight
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Modifier

newtype BreathOfTheSleeper = BreathOfTheSleeper AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breathOfTheSleeper :: AssetCard BreathOfTheSleeper
breathOfTheSleeper = asset BreathOfTheSleeper Cards.breathOfTheSleeper

instance HasAbilities BreathOfTheSleeper where
  getAbilities (BreathOfTheSleeper a) =
    [ restricted a 1 ControlsThis fightAction_
    , controlled a 2 (DuringSkillTest $ SkillTestOnAsset (be a))
        $ ConstantReaction "Spend Charges" (WouldRevealChaosTokens #when You) (UseCostUpTo (be a) Charge 1 3)
    ]

instance RunMessage BreathOfTheSleeper where
  runMessage msg a@(BreathOfTheSleeper attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      let source = attrs.ability 1
      onSucceedByEffect sid (atLeast 0) (attrs.ability 1) sid $ doStep 1 msg
      aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pure
        $ BreathOfTheSleeper
        $ setMetaKey "breathIgnored" ([] :: [ChaosTokenFace])
        $ setMetaKey "breathSelected" ([] :: [ChaosTokenFace]) attrs
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      let selected :: [ChaosTokenFace] = getMetaKeyDefault "breathSelected" [] attrs
      let ignored :: [ChaosTokenFace] = getMetaKeyDefault "breathIgnored" [] attrs
      when (any (`elem` ignored) selected) do
        withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DamageDealt 2)
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (getDrawSource -> drawSource) (totalUsesPayment -> n) -> do
      push
        $ ReplaceCurrentDraw drawSource iid
        $ Choose (toSource attrs) 1 ResolveChoice (Undecided Draw : replicate n (Undecided Draw)) [] Nothing
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure a
    ChaosTokenSelected _ (isSource attrs -> True) chaosToken -> do
      let otherTokens = getMetaKeyDefault "breathSelected" [] attrs
      pure $ BreathOfTheSleeper $ setMetaKey "breathSelected" (chaosToken.face : otherTokens) attrs
    ChaosTokenIgnored _ (isSource attrs -> True) chaosToken -> do
      let otherTokens = getMetaKeyDefault "breathIgnored" [] attrs
      pure $ BreathOfTheSleeper $ setMetaKey "breathIgnored" (chaosToken.face : otherTokens) attrs
    _ -> BreathOfTheSleeper <$> liftRunMessage msg attrs
