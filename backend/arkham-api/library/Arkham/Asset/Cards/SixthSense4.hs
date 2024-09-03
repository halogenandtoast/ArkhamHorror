module Arkham.Asset.Cards.SixthSense4 (sixthSense4, sixthSense4Effect, SixthSense4 (..)) where

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

newtype SixthSense4 = SixthSense4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sixthSense4 :: AssetCard SixthSense4
sixthSense4 = asset SixthSense4 Cards.sixthSense4

instance HasAbilities SixthSense4 where
  getAbilities (SixthSense4 a) = [investigateAbility a 1 mempty ControlsThis]

instance RunMessage SixthSense4 where
  runMessage msg a@(SixthSense4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      lid <- getJustLocation iid
      sid <- getRandom
      investigation <-
        aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)

      pushAll
        $ [ createCardEffect Cards.sixthSense4 (effectMetaTarget sid) source (InvestigationTarget iid lid)
          , skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower 2)
          ]
        <> leftOr investigation
      pure a
    _ -> SixthSense4 <$> runMessage msg attrs

newtype SixthSense4Effect = SixthSense4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sixthSense4Effect :: EffectArgs -> SixthSense4Effect
sixthSense4Effect = cardEffect SixthSense4Effect Cards.sixthSense4

instance RunMessage SixthSense4Effect where
  runMessage msg e@(SixthSense4Effect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken (SkillTestSource sid) iid token | maybe False (isTarget sid) attrs.metaTarget -> case effectTarget of
      InvestigationTarget iid' lid | iid == iid' -> do
        when (chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing]) $ do
          currentShroud <- fieldJust LocationShroud lid
          locations <-
            selectWithField
              LocationShroud
              ( RevealedLocation
                  <> LocationMatchAny
                    [ LocationWithDistanceFrom n Anywhere
                    | n <- [1 .. 2]
                    ]
              )
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
                            , SetSkillTestTarget
                                (BothTarget (toTarget location) (toTarget lid))
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
    _ -> SixthSense4Effect <$> runMessage msg attrs
