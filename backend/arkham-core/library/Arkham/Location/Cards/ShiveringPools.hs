module Arkham.Location.Cards.ShiveringPools
  ( shiveringPools
  , ShiveringPools(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Investigator.Types ( Field(..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers
import Arkham.Timing qualified as Timing

newtype ShiveringPools = ShiveringPools LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shiveringPools :: LocationCard ShiveringPools
shiveringPools = locationWith
  ShiveringPools
  Cards.shiveringPools
  5
  (PerPlayer 1)
  NoSymbol
  []
  ((connectsToL .~ adjacentLocations)
  . (costToEnterUnrevealedL
    .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation]
    )
  )

instance HasAbilities ShiveringPools where
  getAbilities (ShiveringPools attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ restrictedAbility attrs 1 Here $ ForcedAbility $ TurnEnds Timing.After You
        , restrictedAbility
            attrs
            2
            (AnyCriterion
              [ Negate
                  (LocationExists
                  $ LocationInDirection dir (LocationWithId $ toId attrs)
                  )
              | dir <- [Below, RightOf]
              ]
            )
          $ ForcedAbility
          $ RevealLocation Timing.When Anyone
          $ LocationWithId
          $ toId attrs
        ]
      else []

instance RunMessage ShiveringPools where
  runMessage msg l@(ShiveringPools attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) _ 1 _ -> do
      hasResources <- fieldP InvestigatorResources (> 0) iid
      push
        $ chooseOrRunOne iid
        $ Label
            "Take 1 direct damage"
            [InvestigatorDirectDamage iid (toSource attrs) 1 0]
        : [ Label
              "Lose 5 resources"
              [LoseResources iid 5]
          | hasResources
          ]
      pure l
    UseCardAbility iid (isSource attrs -> True) _ 2 _ -> do
      push (DrawFromScenarioDeck iid CatacombsDeck (toTarget attrs) 1)
      pure l
    DrewFromScenarioDeck iid _ (isTarget attrs -> True) cards -> do
      case cards of
        [card] -> do
          placeBelow <- placeAtDirection Below attrs <*> pure card
          placeRight <- placeAtDirection RightOf attrs <*> pure card
          belowEmpty <- directionEmpty attrs Below
          rightEmpty <- directionEmpty attrs RightOf
          push
            $ chooseOrRunOne iid
            $ [ Label "Place Below" placeBelow | belowEmpty ]
            <> [ Label "Place to the Right" placeRight | rightEmpty ]
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    _ -> ShiveringPools <$> runMessage msg attrs
