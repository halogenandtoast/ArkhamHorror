module Arkham.Asset.Cards.AlyssaGraham
  ( alyssaGraham
  , AlyssaGraham(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target

newtype AlyssaGraham = AlyssaGraham AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alyssaGraham :: AssetCard AlyssaGraham
alyssaGraham = ally AlyssaGraham Cards.alyssaGraham (1, 3)

instance HasAbilities AlyssaGraham where
  getAbilities (AlyssaGraham a) =
    [ restrictedAbility a 1 ControlsThis $ FastAbility $ Costs
        [ExhaustCost (toTarget a)]
    ]

instance HasModifiersFor AlyssaGraham where
  getModifiersFor _ (InvestigatorTarget iid) (AlyssaGraham a) =
    pure [ toModifier a (SkillModifier SkillIntellect 1) | controlledBy a iid ]
  getModifiersFor _ _ _ = pure []


instance RunMessage AlyssaGraham where
  runMessage msg a@(AlyssaGraham attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      targets <- selectListMap InvestigatorTarget Anyone
      let
        search target = TargetLabel
          target
          [ Search
              iid
              source
              target
              [fromTopOfDeck 1]
              AnyCard
              (DeferSearchedToTarget $ toTarget attrs)
          ]
      push $ chooseOne iid $ search EncounterDeckTarget : map search targets
      pure a
    SearchFound iid target deck cards | isTarget attrs target -> a <$ pushAll
      [ FocusCards cards
      , chooseOne
        iid
        [ Label
          "Add 1 Doom to Alyssa to move card to bottom"
          [ UnfocusCards
          , PlaceDoom (toTarget attrs) 1
          , MoveTopOfDeckToBottom (toSource attrs) deck 1
          ]
        , Label "Leave card on top" [UnfocusCards]
        ]
      ]
    _ -> AlyssaGraham <$> runMessage msg attrs
