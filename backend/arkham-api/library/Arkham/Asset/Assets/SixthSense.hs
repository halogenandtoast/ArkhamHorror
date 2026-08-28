module Arkham.Asset.Assets.SixthSense (sixthSense, sixthSenseEffect) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFace)
import Arkham.Helpers.Cost
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Investigate
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window qualified as Window

newtype SixthSense = SixthSense AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sixthSense :: AssetCard SixthSense
sixthSense = asset SixthSense Cards.sixthSense

instance HasAbilities SixthSense where
  getAbilities (SixthSense a) = [investigateAbility a 1 mempty ControlsThis]

instance RunMessage SixthSense where
  runMessage msg a@(SixthSense attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        let source = attrs.ability 1
        sid <- getRandom
        createSkillTestCardEffect sid Cards.sixthSense Nothing source (InvestigationTarget iid lid)
        aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)
      pure a
    _ -> SixthSense <$> liftRunMessage msg attrs

newtype SixthSenseEffect = SixthSenseEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sixthSenseEffect :: EffectArgs -> SixthSenseEffect
sixthSenseEffect = cardEffect SixthSenseEffect Cards.sixthSense

instance RunMessage SixthSenseEffect where
  runMessage msg e@(SixthSenseEffect attrs) = runQueueT $ case msg of
    RevealChaosToken (SkillTestSource sid) iid token
      | Just sid == attrs.skillTest
      , not attrs.finished -> do
          faces <- getModifiedChaosTokenFace token
          -- The latch closes in DoStep 1, not here: a finished effect stops
          -- receiving messages entirely (Arkham.Effect, RunMessage Effect), so
          -- setting it now would swallow the DoStep we are about to queue.
          when (any (`elem` [Skull, Cultist, Tablet, ElderThing]) faces) do
            priority $ push $ If (Window.RevealChaosTokenEffect iid token attrs.id) [DoStep 1 msg]
          pure e
    DoStep 1 (RevealChaosToken (SkillTestSource sid) iid _) | Just sid == attrs.skillTest -> do
      case attrs.target of
        InvestigationTarget iid' lid | iid == iid' -> do
          currentShroud <- fieldJust LocationShroud lid
          locations <-
            selectWithField
              LocationShroud
              (connectedFrom (locationWithInvestigator iid) <> RevealedLocation)
              <&> mapMaybe (\(loc, mshroud) -> (loc,) <$> mshroud)
          locationsWithAdditionalCosts <- forMaybeM locations \location@(lid', _) -> do
            mods <- getModifiers lid'
            let costs = fold [m | AdditionalCostToInvestigate m <- mods]
            canAfford <- getCanAffordCost iid attrs [#investigate] [] costs
            pure $ guard canAfford $> (location, costs)
          batchId <- getRandom
          chooseOneM iid do
            labeledI "doNotChooseOtherLocation" nothing
            for_ locationsWithAdditionalCosts \((location, shroud), cost) -> do
              targeting location do
                batching batchId do
                  push $ PayAdditionalCost iid batchId cost
                  push $ SetSkillTestTarget (toTarget location)
                  skillTestModifier sid attrs.source iid (AsIfAt location)
                  chooseOneM iid do
                    labeledI "useNewLocationShroud" do
                      skillTestModifier sid attrs.source sid (SetDifficulty shroud)

                    labeledI "useOriginalLocationsShroud" do
                      skillTestModifier sid attrs.source sid (SetDifficulty currentShroud)
        _ -> error "Invalid target"
      pure . SixthSenseEffect $ finishedEffect attrs
    RepeatSkillTest _ stId
      | Just stId == attrs.skillTest ->
          SixthSenseEffect <$> liftRunMessage msg (unfinishedEffect attrs)
    _ -> SixthSenseEffect <$> liftRunMessage msg attrs
