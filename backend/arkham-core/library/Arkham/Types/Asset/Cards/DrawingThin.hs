module Arkham.Types.Asset.Cards.DrawingThin
  ( drawingThin
  , DrawingThin(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype DrawingThin = DrawingThin AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawingThin :: AssetCard DrawingThin
drawingThin = asset DrawingThin Cards.drawingThin

instance HasAbilities DrawingThin where
  getAbilities (DrawingThin a) =
    [ restrictedAbility a 1 OwnsThis
        $ ReactionAbility
            (InitiatedSkillTest Timing.When You AnySkillTest AnyValue)
        $ ExhaustCost (toTarget a)
    ]

instance AssetRunner env => RunMessage env DrawingThin where
  runMessage msg a@(DrawingThin attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ CreateWindowModifierEffect
        EffectSkillTestWindow
        (EffectModifiers $ toModifiers attrs [Difficulty 2])
        source
        SkillTestTarget
      , chooseOne
        iid
        [ Label "Take 2 resources" [TakeResources iid 2 False]
        , Label "Draw 1 card" [DrawCards iid 1 False]
        ]
      ]
    _ -> DrawingThin <$> runMessage msg attrs
