module Arkham.Asset.Cards.SixthSense (
  sixthSense,
  sixthSenseEffect,
  SixthSense (..),
) where

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
      investigation <-
        aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate iid source)

      pushAll
        $ createCardEffect Cards.sixthSense Nothing source (InvestigationTarget iid lid)
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
    RevealChaosToken _ iid token -> case effectTarget of
      InvestigationTarget iid' lid | iid == iid' -> do
        when (chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing]) $ do
          currentShroud <- field LocationShroud lid
          locations <-
            selectWithField LocationShroud
              $ ConnectedLocation
              <> RevealedLocation
          pushAll
            [ If
                (Window.RevealChaosTokenEffect iid token effectId)
                [ chooseOne iid
                    $ Label "Do not choose other location" []
                    : [ targetLabel
                        location
                        [ SetSkillTestTarget (toTarget location)
                        , chooseOne
                            iid
                            [ Label
                                "Use new location's shroud"
                                [ skillTestModifier
                                    (AbilitySource effectSource 1)
                                    SkillTestTarget
                                    (SetDifficulty shroud)
                                ]
                            , Label
                                "Use original locations shroud"
                                [ skillTestModifier
                                    (AbilitySource effectSource 1)
                                    SkillTestTarget
                                    (SetDifficulty currentShroud)
                                ]
                            ]
                        ]
                      | (location, shroud) <- locations
                      ]
                ]
            , DisableEffect effectId
            ]
        pure e
      _ -> error "Invalid target"
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> SixthSenseEffect <$> runMessage msg attrs
