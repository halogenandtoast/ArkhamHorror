module Arkham.Asset.Assets.EyesOfTheDreamer (eyesOfTheDreamer) where

import Arkham.Ability
import Arkham.Aspect.Types
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosBagStepState
import Arkham.ChaosToken.Types
import Arkham.Investigate
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Modifier

newtype EyesOfTheDreamer = EyesOfTheDreamer AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyesOfTheDreamer :: AssetCard EyesOfTheDreamer
eyesOfTheDreamer = asset EyesOfTheDreamer Cards.eyesOfTheDreamer

instance HasAbilities EyesOfTheDreamer where
  getAbilities (EyesOfTheDreamer a) =
    [ restricted a 1 ControlsThis investigateAction_
    , controlled a 2 (DuringSkillTest $ SkillTestOnAsset (be a))
        $ ConstantReaction "Spend Charges" (WouldRevealChaosTokens #when You) (UseCostUpTo (be a) Charge 1 3)
    ]

instance RunMessage EyesOfTheDreamer where
  runMessage msg a@(EyesOfTheDreamer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      let source = attrs.ability 1
      onSucceedByEffect sid (atLeast 0) (attrs.ability 1) sid $ doStep 1 msg
      aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)
      pure
        $ EyesOfTheDreamer
        $ setMetaKey "eyesIgnored" ([] :: [ChaosTokenFace])
        $ setMetaKey "eyesSelected" ([] :: [ChaosTokenFace]) attrs
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      let selected :: [ChaosTokenFace] = getMetaKeyDefault "eyesSelected" [] attrs
      let ignored :: [ChaosTokenFace] = getMetaKeyDefault "eyesIgnored" [] attrs
      when (any (`elem` ignored) selected) do
        withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (getDrawSource -> drawSource) (totalUsesPayment -> n) -> do
      push
        $ ReplaceCurrentDraw drawSource iid
        $ Choose (toSource attrs) 1 ResolveChoice (Undecided Draw : replicate n (Undecided Draw)) [] Nothing
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure a
    ChaosTokenSelected _ (isSource attrs -> True) chaosToken -> do
      let otherTokens = getMetaKeyDefault "eyesSelected" [] attrs
      pure $ EyesOfTheDreamer $ setMetaKey "eyesSelected" (chaosToken.face : otherTokens) attrs
    ChaosTokenIgnored _ (isSource attrs -> True) chaosToken -> do
      let otherTokens = getMetaKeyDefault "eyesIgnored" [] attrs
      pure $ EyesOfTheDreamer $ setMetaKey "eyesIgnored" (chaosToken.face : otherTokens) attrs
    _ -> EyesOfTheDreamer <$> liftRunMessage msg attrs
