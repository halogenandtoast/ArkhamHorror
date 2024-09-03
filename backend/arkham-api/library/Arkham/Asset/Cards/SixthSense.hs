module Arkham.Asset.Cards.SixthSense (sixthSense, sixthSenseEffect, SixthSense (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Helpers.Investigator
import Arkham.Investigate
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Prelude
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
  runMessage msg a@(SixthSense attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      lid <- getJustLocation iid
      sid <- getRandom
      investigation <-
        aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)

      pushAll
        $ createCardEffect Cards.sixthSense (effectMetaTarget sid) source (InvestigationTarget iid lid)
        : leftOr investigation
      pure a
    _ -> SixthSense <$> runMessage msg attrs

newtype SixthSenseEffect = SixthSenseEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sixthSenseEffect :: EffectArgs -> SixthSenseEffect
sixthSenseEffect = cardEffect SixthSenseEffect Cards.sixthSense

instance RunMessage SixthSenseEffect where
  runMessage msg e@(SixthSenseEffect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken (SkillTestSource sid) iid token | maybe False (isTarget sid) attrs.metaTarget -> case effectTarget of
      InvestigationTarget iid' lid | iid == iid' -> do
        when (chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing]) $ do
          currentShroud <- fieldJust LocationShroud lid
          locations <-
            selectWithField LocationShroud (ConnectedLocation <> RevealedLocation)
              <&> mapMaybe (\(loc, mshroud) -> (loc,) <$> mshroud)
          locationsWithAdditionalCosts <- forMaybeM locations \location@(lid', _) -> do
            mods <- getModifiers lid'
            let costs = fold [m | AdditionalCostToInvestigate m <- mods]
            canAfford <- getCanAffordCost iid attrs [#investigate] [] costs
            pure $ guard canAfford $> (location, costs)
          player <- getPlayer iid
          batchId <- getRandom
          pushAll
            [ If
                (Window.RevealChaosTokenEffect iid token effectId)
                [ chooseOne player
                    $ Label "Do not choose other location" []
                    : [ targetLabel
                        location
                        [ Would
                            batchId
                            [ PayAdditionalCost iid batchId cost
                            , SetSkillTestTarget (toTarget location)
                            , chooseOne
                                player
                                [ Label
                                    "Use new location's shroud"
                                    [skillTestModifier sid (AbilitySource effectSource 1) sid (SetDifficulty shroud)]
                                , Label
                                    "Use original locations shroud"
                                    [skillTestModifier sid (AbilitySource effectSource 1) sid (SetDifficulty currentShroud)]
                                ]
                            ]
                        ]
                      | ((location, shroud), cost) <- locationsWithAdditionalCosts
                      ]
                ]
            , DisableEffect effectId
            ]
        pure e
      _ -> error "Invalid target"
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> e <$ push (DisableEffect effectId)
    _ -> SixthSenseEffect <$> runMessage msg attrs
