module Arkham.Asset.Cards.DrawingThin
  ( drawingThin
  , DrawingThin(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype DrawingThin = DrawingThin AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawingThin :: AssetCard DrawingThin
drawingThin = asset DrawingThin Cards.drawingThin

instance HasAbilities DrawingThin where
  getAbilities (DrawingThin a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
            (InitiatedSkillTest Timing.When You AnySkillType AnySkillTestValue)
        $ ExhaustCost (toTarget a)
    ]

instance RunMessage DrawingThin where
  runMessage msg a@(DrawingThin attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      drawing <- drawCards iid attrs 1
      pushAll
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [Difficulty 2])
          source
          SkillTestTarget
        , chooseOne
          iid
          [ Label "Take 2 resources" [TakeResources iid 2 (toAbilitySource attrs 1) False]
          , Label "Draw 1 card" [drawing]
          ]
        ]
      pure a
    _ -> DrawingThin <$> runMessage msg attrs
