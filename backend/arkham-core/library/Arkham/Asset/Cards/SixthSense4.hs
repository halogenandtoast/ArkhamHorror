module Arkham.Asset.Cards.SixthSense4 (
  sixthSense4,
  sixthSense4Effect,
  SixthSense4 (..),
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
import Arkham.SkillType
import Arkham.Window qualified as Window

newtype SixthSense4 = SixthSense4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

sixthSense4 :: AssetCard SixthSense4
sixthSense4 = asset SixthSense4 Cards.sixthSense4

instance HasAbilities SixthSense4 where
  getAbilities (SixthSense4 a) = [investigateAbility a 1 mempty ControlsThis]

instance RunMessage SixthSense4 where
  runMessage msg a@(SixthSense4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      lid <- getJustLocation iid
      investigation <-
        aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate iid source)

      pushAll
        $ [ createCardEffect Cards.sixthSense4 Nothing source (InvestigationTarget iid lid)
          , skillTestModifier (toAbilitySource attrs 1) iid (SkillModifier SkillWillpower 2)
          ]
        <> leftOr investigation
      pure a
    _ -> SixthSense4 <$> runMessage msg attrs

newtype SixthSense4Effect = SixthSense4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

sixthSense4Effect :: EffectArgs -> SixthSense4Effect
sixthSense4Effect = cardEffect SixthSense4Effect Cards.sixthSense4

instance RunMessage SixthSense4Effect where
  runMessage msg e@(SixthSense4Effect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token -> case effectTarget of
      InvestigationTarget iid' lid | iid == iid' -> do
        when (chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing]) $ do
          currentShroud <- field LocationShroud lid
          locations <-
            selectWithField LocationShroud
              $ RevealedLocation
              <> LocationMatchAny
                [ LocationWithDistanceFrom n Anywhere
                | n <- [1 .. 2]
                ]
          player <- getPlayer iid
          pushAll
            [ If
                (Window.RevealChaosTokenEffect iid token effectId)
                [ chooseOne player
                    $ Label "Do not choose other location" []
                    : [ targetLabel
                        location
                        [ SetSkillTestTarget
                            (BothTarget (toTarget location) (toTarget lid))
                        , chooseOne
                            player
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
    _ -> SixthSense4Effect <$> runMessage msg attrs
