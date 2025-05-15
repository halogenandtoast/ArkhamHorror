module Arkham.Asset.Assets.PocketMultiTool (pocketMultiTool) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Customization
import Arkham.Helpers.SkillTest (
  getSkillTestSource,
  inAttackSkillTest,
  inEvasionSkillTest,
  isInvestigation,
  withSkillTest,
 )
import Arkham.Matcher
import Arkham.Modifier

newtype PocketMultiTool = PocketMultiTool AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pocketMultiTool :: AssetCard PocketMultiTool
pocketMultiTool = asset PocketMultiTool Cards.pocketMultiTool

instance HasAbilities PocketMultiTool where
  getAbilities (PocketMultiTool a) =
    [ restricted a 1 (detachableCriteria <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ if a `hasCustomization` SpringLoaded
          then triggered (WouldHaveSkillTestResult #when You #any #failure) (exhaust a)
          else FastAbility (exhaust a)
    ]
   where
    detachableCriteria = if a `hasCustomization` Detachable then OnSameLocation else ControlsThis

instance RunMessage PocketMultiTool where
  runMessage msg a@(PocketMultiTool attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pryBar <- runDefaultMaybeT 0 do
        guard $ attrs `hasCustomization` PryBar
        source <- MaybeT getSkillTestSource
        guard $ isJust source.treachery
        pure 1
      sharpenedKnife <- runDefaultMaybeT 0 do
        guard $ attrs `hasCustomization` SharpenedKnife
        liftGuardM inAttackSkillTest
        pure 1
      signalMirror <- runDefaultMaybeT 0 do
        guard $ attrs `hasCustomization` SignalMirror
        liftGuardM inEvasionSkillTest
        pure 1
      magnifyingLens <- runDefaultMaybeT 0 do
        guard $ attrs `hasCustomization` MagnifyingLens
        liftGuardM isInvestigation
        pure 1

      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) iid
          $ AnySkillValue (1 + pryBar + sharpenedKnife + signalMirror + magnifyingLens)
      pushWhen (attrs `hasCustomization` SpringLoaded) RerunSkillTest
      pure a
    FailedThisSkillTest iid _ | attrs `controlledBy` iid -> do
      when (attrs.exhausted && attrs `hasCustomization` LuckyCharm) $ push $ Ready (toTarget attrs)
      pure a
    _ -> PocketMultiTool <$> liftRunMessage msg attrs
