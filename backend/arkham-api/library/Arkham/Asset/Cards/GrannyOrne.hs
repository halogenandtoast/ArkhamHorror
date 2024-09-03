module Arkham.Asset.Cards.GrannyOrne (grannyOrne, GrannyOrne (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype GrannyOrne = GrannyOrne AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grannyOrne :: AssetCard GrannyOrne
grannyOrne = ally GrannyOrne Cards.grannyOrne (1, 3)

instance HasModifiersFor GrannyOrne where
  getModifiersFor (InvestigatorTarget iid) (GrannyOrne a)
    | controlledBy a iid = pure $ toModifiers a [SkillModifier #willpower 1]
  getModifiersFor _ _ = pure []

instance HasAbilities GrannyOrne where
  getAbilities (GrannyOrne a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (WouldHaveSkillTestResult #when (affectsOthers $ InvestigatorAt YourLocation) #any #failure)
          (exhaust a)
    ]

instance RunMessage GrannyOrne where
  runMessage msg a@(GrannyOrne attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      player <- getPlayer iid
      withSkillTest \sid ->
        push
          $ chooseOne
            player
            [ Label
                "Fail by 1 less"
                [ skillTestModifier sid (attrs.ability 1) sid (SkillTestResultValueModifier (-1))
                , RecalculateSkillTestResults
                ]
            , Label
                "Fail by 1 more"
                [ skillTestModifier sid (attrs.ability 1) sid (SkillTestResultValueModifier 1)
                , RecalculateSkillTestResults
                ]
            ]
      pure a
    _ -> GrannyOrne <$> runMessage msg attrs
