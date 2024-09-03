module Arkham.Location.Cards.Restaurant (
  restaurant,
  restaurantEffect,
  Restaurant (..),
)
where

import Arkham.Prelude

import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype Restaurant = Restaurant LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restaurant :: LocationCard Restaurant
restaurant = location Restaurant Cards.restaurant 4 (PerPlayer 1)

instance HasAbilities Restaurant where
  getAbilities (Restaurant attrs) =
    withRevealedAbilities
      attrs
      [ playerLimit PerGame $ restrictedAbility attrs 1 Here $ actionAbilityWithCost $ ResourceCost 2
      , restrictedAbility
          attrs
          2
          (Here <> AllLocationsMatch Anywhere (RevealedLocation <> LocationWithoutClues))
          $ FastAbility Free
      ]

instance RunMessage Restaurant where
  runMessage msg l@(Restaurant attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ createCardEffect Cards.restaurant Nothing (toAbilitySource attrs 1) iid
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      n <- perPlayer 1
      push $ PlaceClues (toAbilitySource attrs 1) (toTarget attrs) n
      pure l
    _ -> Restaurant <$> runMessage msg attrs

newtype RestaurantEffect = RestaurantEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restaurantEffect :: EffectArgs -> RestaurantEffect
restaurantEffect = cardEffect RestaurantEffect Cards.restaurant

instance RunMessage RestaurantEffect where
  runMessage msg e@(RestaurantEffect attrs) = case msg of
    EndTurn iid | iid `is` attrs.target -> do
      isRestaurant <-
        maybe (pure False) (<=~> locationIs Cards.restaurant) =<< field InvestigatorLocation iid
      if isRestaurant
        then do
          player <- getPlayer iid
          pushAll
            [ chooseOne
                player
                [ Label
                    "Take 1 Horror to heal 3 damage"
                    [assignHorror iid (effectSource attrs) 1, HealDamage (toTarget iid) (effectSource attrs) 3]
                , Label "Skip" []
                ]
            , disable attrs
            ]
        else push $ disable attrs
      pure e
    _ -> RestaurantEffect <$> runMessage msg attrs
