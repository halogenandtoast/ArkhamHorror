module Arkham.Asset.Cards.SixthSense (
  sixthSense,
  sixthSenseEffect,
  SixthSense (..),
) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Helpers.Investigator
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Token
import Arkham.Window qualified as Window

newtype SixthSense = SixthSense AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sixthSense :: AssetCard SixthSense
sixthSense = asset SixthSense Cards.sixthSense

instance HasAbilities SixthSense where
  getAbilities (SixthSense a) =
    [ restrictedAbility a 1 ControlsThis $
        ActionAbility (Just Action.Investigate) $
          ActionCost 1
    ]

instance RunMessage SixthSense where
  runMessage msg a@(SixthSense attrs) = case msg of
    UseCardAbility iid source@(isSource attrs -> True) 1 _ _ -> do
      lid <- getJustLocation iid
      skillType <- field LocationInvestigateSkill lid
      pushAll
        [ createCardEffect Cards.sixthSense Nothing source (InvestigationTarget iid lid)
        , Investigate
            iid
            lid
            source
            Nothing
            (if skillType == SkillIntellect then SkillWillpower else skillType)
            False
        ]
      pure a
    _ -> SixthSense <$> runMessage msg attrs

newtype SixthSenseEffect = SixthSenseEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sixthSenseEffect :: EffectArgs -> SixthSenseEffect
sixthSenseEffect = cardEffect SixthSenseEffect Cards.sixthSense

instance RunMessage SixthSenseEffect where
  runMessage msg e@(SixthSenseEffect attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token -> case effectTarget of
      InvestigationTarget iid' lid | iid == iid' -> do
        when (tokenFace token `elem` [Skull, Cultist, Tablet, ElderThing]) $ do
          currentShroud <- field LocationShroud lid
          locations <- selectWithField LocationShroud $ ConnectedLocation <> RevealedLocation
          pushAll
            [ If
                (Window.RevealTokenEffect iid token effectId)
                [chooseOne iid $ Label "Do not choose other location" [] : [targetLabel location [SetSkillTestTarget (toTarget location), chooseOne iid [Label "Use new location's shroud" [skillTestModifier (AbilitySource effectSource 1) SkillTestTarget (SetDifficulty shroud)], Label "Use original locations shroud" [skillTestModifier (AbilitySource effectSource 1) SkillTestTarget (SetDifficulty currentShroud)]]] | (location, shroud) <- locations]]
            , DisableEffect effectId
            ]
        pure e
      _ -> error "Invalid target"
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> SixthSenseEffect <$> runMessage msg attrs
