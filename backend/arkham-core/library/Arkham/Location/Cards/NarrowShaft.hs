module Arkham.Location.Cards.NarrowShaft
  ( narrowShaft
  , NarrowShaft(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.EffectMetadata
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype NarrowShaft = NarrowShaft LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narrowShaft :: LocationCard NarrowShaft
narrowShaft = locationWith
  NarrowShaft
  Cards.narrowShaft
  2
  (PerPlayer 1)
  ((connectsToL .~ adjacentLocations)
  . (costToEnterUnrevealedL
    .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation]
    )
  )

instance HasAbilities NarrowShaft where
  getAbilities (NarrowShaft attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ mkAbility attrs 1 $ ForcedAbility $ Moves
          Timing.When
          You
          AnySource
          (LocationWithId $ toId attrs)
          UnrevealedLocation
        , restrictedAbility
          attrs
          2
          (AnyCriterion
            [ Negate
                (LocationExists
                $ LocationInDirection dir (LocationWithId $ toId attrs)
                )
            | dir <- [Above, Below, RightOf]
            ]
          )
        $ ForcedAbility
        $ RevealLocation Timing.When Anyone
        $ LocationWithId
        $ toId attrs
        ]
      else []

instance RunMessage NarrowShaft where
  runMessage msg l@(NarrowShaft attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      moveFrom <- popMessageMatching \case
        MoveFrom _ iid' lid' -> iid' == iid && toId l == lid'
        _ -> False
      moveTo <- popMessageMatching \case
        MoveTo _ iid' _ -> iid == iid' -- we don't know where they are going for the cancel
        _ -> False
      let
        target = InvestigatorTarget iid
        effectMetadata = Just $ EffectMessages (catMaybes [moveFrom, moveTo])
      pushAll
        [ CreateEffect "03254" effectMetadata (toSource attrs) target
        , beginSkillTest iid (toSource attrs) target Nothing SkillAgility 3
        ]
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push (DrawFromScenarioDeck iid CatacombsDeck (toTarget attrs) 1)
      pure l
    DrewFromScenarioDeck iid _ (isTarget attrs -> True) cards -> do
      case cards of
        [card] -> do
          placeAbove <- placeAtDirection Above attrs <*> pure card
          placeRight <- placeAtDirection RightOf attrs <*> pure card
          aboveEmpty <- directionEmpty attrs Above
          rightEmpty <- directionEmpty attrs RightOf
          push
            $ chooseOrRunOne iid
            $ [ Label "Place Above" placeAbove | aboveEmpty ]
            <> [ Label "Place to the Right" placeRight | rightEmpty ]
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    _ -> NarrowShaft <$> runMessage msg attrs
