module Arkham.Location.Cards.ShiveringPools (shiveringPools, ShiveringPools (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype ShiveringPools = ShiveringPools LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shiveringPools :: LocationCard ShiveringPools
shiveringPools =
  locationWith ShiveringPools Cards.shiveringPools 5 (PerPlayer 1)
    $ (connectsToL .~ adjacentLocations)
    . ( costToEnterUnrevealedL
          .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation]
      )

instance HasAbilities ShiveringPools where
  getAbilities (ShiveringPools attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 Here $ forced $ TurnEnds #after You
      , restrictedAbility
          attrs
          2
          (oneOf [notExists $ LocationInDirection dir (be attrs) | dir <- [Below, RightOf]])
          $ forced
          $ RevealLocation #when Anyone (be attrs)
      ]

instance RunMessage ShiveringPools where
  runMessage msg l@(ShiveringPools attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasResources <- fieldP InvestigatorResources (> 0) iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label "Take 1 direct damage" [directDamage iid (attrs.ability 1) 1]
        : [ Label "Lose 5 resources" [LoseResources iid (attrs.ability 1) 5]
          | hasResources
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck 1
      pure l
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      case drewCards.cards of
        [card] -> do
          placeBelow <- placeAtDirection Below attrs >>= \f -> f card
          placeRight <- placeAtDirection RightOf attrs >>= \f -> f card
          belowEmpty <- directionEmpty attrs Below
          rightEmpty <- directionEmpty attrs RightOf
          player <- getPlayer iid
          push
            $ chooseOrRunOne player
            $ [Label "Place Below" placeBelow | belowEmpty]
            <> [Label "Place to the Right" placeRight | rightEmpty]
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    _ -> ShiveringPools <$> runMessage msg attrs
