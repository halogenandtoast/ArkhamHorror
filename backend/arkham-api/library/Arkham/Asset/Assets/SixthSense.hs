module Arkham.Asset.Assets.SixthSense (sixthSense, sixthSenseEffect, SixthSense (..)) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Classes.HasQueue (evalQueueT)
import Arkham.Effect.Import
import Arkham.Helpers.Cost
import Arkham.Helpers.Investigator
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
      let source = attrs.ability 1
      lid <- getJustLocation iid
      sid <- getRandom
      createCardEffect Cards.sixthSense (effectMetaTarget sid) source (InvestigationTarget iid lid)
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
    RevealChaosToken (SkillTestSource sid) iid token | maybe False (isTarget sid) attrs.metaTarget -> do
      case attrs.target of
        InvestigationTarget iid' lid | iid == iid' -> do
          when (token.face `elem` [Skull, Cultist, Tablet, ElderThing]) $ do
            currentShroud <- fieldJust LocationShroud lid
            locations <-
              selectWithField LocationShroud (ConnectedLocation <> RevealedLocation)
                <&> mapMaybe (\(loc, mshroud) -> (loc,) <$> mshroud)
            locationsWithAdditionalCosts <- forMaybeM locations \location@(lid', _) -> do
              mods <- getModifiers lid'
              let costs = fold [m | AdditionalCostToInvestigate m <- mods]
              canAfford <- getCanAffordCost iid attrs [#investigate] [] costs
              pure $ guard canAfford $> (location, costs)
            batchId <- getRandom
            askQ <- evalQueueT $ chooseOneM iid do
              labeled "Do not choose other location" nothing
              for_ locationsWithAdditionalCosts \((location, shroud), cost) -> do
                targeting location do
                  batching batchId do
                    push $ PayAdditionalCost iid batchId cost
                    push $ SetSkillTestTarget (toTarget location)
                    chooseOneM iid do
                      labeled "Use new location's shroud" do
                        skillTestModifier sid attrs.source sid (SetDifficulty shroud)

                      labeled "Use original locations shroud" do
                        skillTestModifier sid attrs.source sid (SetDifficulty currentShroud)
            push $ If (Window.RevealChaosTokenEffect iid token attrs.id) askQ
            disable attrs
        _ -> error "Invalid target"
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> disableReturn e
    _ -> SixthSenseEffect <$> liftRunMessage msg attrs
