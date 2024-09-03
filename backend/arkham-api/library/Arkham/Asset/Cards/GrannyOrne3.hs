module Arkham.Asset.Cards.GrannyOrne3 (grannyOrne3, GrannyOrne3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype GrannyOrne3 = GrannyOrne3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grannyOrne3 :: AssetCard GrannyOrne3
grannyOrne3 = ally GrannyOrne3 Cards.grannyOrne3 (1, 3)

instance HasModifiersFor GrannyOrne3 where
  getModifiersFor (InvestigatorTarget iid) (GrannyOrne3 a) | controlledBy a iid = do
    pure $ toModifiers a [SkillModifier #willpower 1, SkillModifier #intellect 1]
  getModifiersFor _ _ = pure []

instance HasAbilities GrannyOrne3 where
  getAbilities (GrannyOrne3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (WouldHaveSkillTestResult #when (affectsOthers $ InvestigatorAt YourLocation) #any #failure)
          (exhaust a)
    ]

instance RunMessage GrannyOrne3 where
  runMessage msg a@(GrannyOrne3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        player <- getPlayer iid
        push
          $ chooseOne
            player
            [ Label
                "Get +1 skill value"
                [ skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
                , RerunSkillTest
                ]
            , Label
                "Get -1 skill value"
                [ skillTestModifier sid (attrs.ability 1) iid (AnySkillValue (-1))
                , RerunSkillTest
                ]
            ]
      pure a
    _ -> GrannyOrne3 <$> runMessage msg attrs
