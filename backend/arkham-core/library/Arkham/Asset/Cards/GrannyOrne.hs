module Arkham.Asset.Cards.GrannyOrne
  ( grannyOrne
  , GrannyOrne(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype GrannyOrne = GrannyOrne AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grannyOrne :: AssetCard GrannyOrne
grannyOrne = ally GrannyOrne Cards.grannyOrne (1, 3)

instance HasModifiersFor GrannyOrne where
  getModifiersFor (InvestigatorTarget iid) (GrannyOrne a)
    | controlledBy a iid = pure $ toModifiers a [SkillModifier SkillWillpower 1]
  getModifiersFor _ _ = pure []

instance HasAbilities GrannyOrne where
  getAbilities (GrannyOrne a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility
        (WouldHaveSkillTestResult
          Timing.When
          (InvestigatorAt YourLocation)
          AnySkillTest
          (FailureResult AnyValue)
        )
        (ExhaustCost $ toTarget a)
    ]

instance RunMessage GrannyOrne where
  runMessage msg a@(GrannyOrne attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ chooseOne
        iid
        [ Label
          "Fail by 1 less"
          [ skillTestModifier
            (toSource attrs)
            SkillTestTarget
            (SkillTestResultValueModifier (-1))
          , RecalculateSkillTestResults
          ]
        , Label
          "Fail by 1 more"
          [ skillTestModifier
            (toSource attrs)
            SkillTestTarget
            (SkillTestResultValueModifier 1)
          , RecalculateSkillTestResults
          ]
        ]
      pure a
    _ -> GrannyOrne <$> runMessage msg attrs
