module Arkham.Asset.Cards.AlyssaGraham (
  alyssaGraham,
  AlyssaGraham (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype AlyssaGraham = AlyssaGraham AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alyssaGraham :: AssetCard AlyssaGraham
alyssaGraham = ally AlyssaGraham Cards.alyssaGraham (1, 3)

instance HasAbilities AlyssaGraham where
  getAbilities (AlyssaGraham a) =
    [ restrictedAbility a 1 ControlsThis
        $ FastAbility
        $ Costs
          [ExhaustCost (toTarget a)]
    ]

instance HasModifiersFor AlyssaGraham where
  getModifiersFor (InvestigatorTarget iid) (AlyssaGraham a) =
    pure [toModifier a (SkillModifier SkillIntellect 1) | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance RunMessage AlyssaGraham where
  runMessage msg a@(AlyssaGraham attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      targets <- map InvestigatorTarget <$> getInvestigatorIds
      let
        goSearch target =
          TargetLabel
            target
            [ Search
                iid
                source
                target
                [fromTopOfDeck 1]
                AnyCard
                (DeferSearchedToTarget $ toTarget attrs)
            ]
      push $ chooseOne iid $ goSearch EncounterDeckTarget : map goSearch targets
      pure a
    SearchFound iid target deck cards | isTarget attrs target -> do
      pushAll
        [ FocusCards cards
        , chooseOne
            iid
            [ Label
                "Add 1 Doom to Alyssa to move card to bottom"
                [ UnfocusCards
                , PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1
                , MoveTopOfDeckToBottom (toSource attrs) deck 1
                ]
            , Label "Leave card on top" [UnfocusCards]
            ]
        ]
      pure a
    _ -> AlyssaGraham <$> runMessage msg attrs
