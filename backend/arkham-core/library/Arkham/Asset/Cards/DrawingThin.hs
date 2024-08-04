module Arkham.Asset.Cards.DrawingThin (drawingThin, DrawingThin (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype DrawingThin = DrawingThin AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawingThin :: AssetCard DrawingThin
drawingThin = asset DrawingThin Cards.drawingThin

instance HasAbilities DrawingThin where
  getAbilities (DrawingThin a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (InitiatedSkillTest #when You AnySkillType AnySkillTestValue #any)
        $ exhaust a
    ]

instance RunMessage DrawingThin where
  runMessage msg a@(DrawingThin attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let drawing = drawCards iid (attrs.ability 1) 1
      player <- getPlayer iid
      pushAll
        [ IncreaseSkillTestDifficulty 2
        , chooseOne
            player
            [ Label "Take 2 resources" [TakeResources iid 2 (toAbilitySource attrs 1) False]
            , Label "Draw 1 card" [drawing]
            ]
        ]
      pure a
    _ -> DrawingThin <$> runMessage msg attrs
