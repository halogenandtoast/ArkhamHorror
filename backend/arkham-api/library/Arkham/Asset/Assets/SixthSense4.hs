module Arkham.Asset.Assets.SixthSense4 (sixthSense4, sixthSense4Effect) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFace)
import Arkham.Helpers.Cost
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Helpers.SkillTest.Target
import Arkham.Investigate
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window qualified as Window

newtype SixthSense4 = SixthSense4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sixthSense4 :: AssetCard SixthSense4
sixthSense4 = asset SixthSense4 Cards.sixthSense4

instance HasAbilities SixthSense4 where
  getAbilities (SixthSense4 a) = [investigateAbility a 1 mempty ControlsThis]

instance RunMessage SixthSense4 where
  runMessage msg a@(SixthSense4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        let source = attrs.ability 1
        sid <- getRandom
        createSkillTestCardEffect sid Cards.sixthSense4 Nothing source (InvestigationTarget iid lid)
        skillTestModifier sid source iid (SkillModifier #willpower 2)
        aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)
      pure a
    _ -> SixthSense4 <$> liftRunMessage msg attrs

newtype SixthSense4Effect = SixthSense4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sixthSense4Effect :: EffectArgs -> SixthSense4Effect
sixthSense4Effect = cardEffect SixthSense4Effect Cards.sixthSense4

instance RunMessage SixthSense4Effect where
  runMessage msg e@(SixthSense4Effect attrs) = runQueueT $ case msg of
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
              (LocationWithDistanceFromAtMost 2 (locationWithInvestigator iid) RevealedLocation)
              <&> mapMaybe (\(loc, mshroud) -> (loc,) <$> mshroud)

          locationsWithAdditionalCosts <- forMaybeM locations \location@(lid', _) -> runMaybeT do
            guard $ lid /= lid'
            mods <- getModifiers lid'
            let costs = fold [m | AdditionalCostToInvestigate m <- mods]
            liftGuardM $ getCanAffordCost iid attrs [#investigate] [] costs
            pure (location, costs)
          batchId <- getRandom
          currentTarget <- fromMaybe (toTarget lid) <$> getSkillTestTarget
          chooseOneM iid do
            labeledI "doNotChooseOtherLocation" nothing
            for_ locationsWithAdditionalCosts \((location, shroud), cost) -> do
              targeting location do
                batching batchId do
                  push $ PayAdditionalCost iid batchId cost
                  push $ SetSkillTestTarget (BothTarget (toTarget location) currentTarget)
                  skillTestModifier sid attrs.source iid (AsIfAlsoAt location)
                  chooseOneM iid do
                    labeledI "useNewLocationShroud" do
                      skillTestModifier sid attrs.source sid (SetDifficulty shroud)
                    labeledI "useOriginalLocationsShroud" do
                      skillTestModifier sid attrs.source sid (SetDifficulty currentShroud)
        _ -> error "Invalid target"
      pure . SixthSense4Effect $ finishedEffect attrs
    RepeatSkillTest _ stId
      | Just stId == attrs.skillTest ->
          SixthSense4Effect <$> liftRunMessage msg (unfinishedEffect attrs)
    _ -> SixthSense4Effect <$> liftRunMessage msg attrs
