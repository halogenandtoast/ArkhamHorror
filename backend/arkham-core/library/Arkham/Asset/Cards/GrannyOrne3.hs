module Arkham.Asset.Cards.GrannyOrne3 (
  grannyOrne3,
  GrannyOrne3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype GrannyOrne3 = GrannyOrne3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grannyOrne3 :: AssetCard GrannyOrne3
grannyOrne3 = ally GrannyOrne3 Cards.grannyOrne3 (1, 3)

instance HasModifiersFor GrannyOrne3 where
  getModifiersFor (InvestigatorTarget iid) (GrannyOrne3 a)
    | controlledBy a iid =
        pure
          $ toModifiers
            a
            [SkillModifier SkillWillpower 1, SkillModifier SkillIntellect 1]
  getModifiersFor _ _ = pure []

instance HasAbilities GrannyOrne3 where
  getAbilities (GrannyOrne3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          ( WouldHaveSkillTestResult
              Timing.When
              (InvestigatorAt YourLocation)
              AnySkillTest
              (FailureResult AnyValue)
          )
          (ExhaustCost $ toTarget a)
    ]

instance RunMessage GrannyOrne3 where
  runMessage msg a@(GrannyOrne3 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push
        $ chooseOne
          iid
          [ Label
              "Get +1 skill value"
              [ skillTestModifier
                  (toSource attrs)
                  (InvestigatorTarget iid)
                  (AnySkillValue 1)
              , RerunSkillTest
              ]
          , Label
              "Get -1 skill value"
              [ skillTestModifier
                  (toSource attrs)
                  (InvestigatorTarget iid)
                  (AnySkillValue (-1))
              , RerunSkillTest
              ]
          ]
      pure a
    _ -> GrannyOrne3 <$> runMessage msg attrs
