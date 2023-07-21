module Arkham.Location.Cards.PassengerCar_167 (
  passengerCar_167,
  PassengerCar_167 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (passengerCar_167)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype PassengerCar_167 = PassengerCar_167 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passengerCar_167 :: LocationCard PassengerCar_167
passengerCar_167 =
  locationWith
    PassengerCar_167
    Cards.passengerCar_167
    1
    (PerPlayer 3)
    (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasModifiersFor PassengerCar_167 where
  getModifiersFor target (PassengerCar_167 l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
        Just leftLocation -> do
          clueCount <- field LocationClues leftLocation
          pure $ toModifiers l [Blocked | not locationRevealed && clueCount > 0]
        Nothing -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities PassengerCar_167 where
  getAbilities (PassengerCar_167 x) =
    withBaseAbilities
      x
      [ restrictedAbility x 1 Here $
        ForcedAbility $
          Enters Timing.After You $
            LocationWithId $
              toId x
      | locationRevealed x
      ]

instance RunMessage PassengerCar_167 where
  runMessage msg l@(PassengerCar_167 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let cost = SkillIconCost 2 (singleton $ SkillIcon SkillAgility)
      hasSkills <-
        getCanAffordCost
          iid
          (toSource attrs)
          Nothing
          [Window Timing.When NonFast]
          cost
      l
        <$ if hasSkills
          then
            push
              ( chooseOne
                  iid
                  [ Label
                      "Take 2 damage"
                      [InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0]
                  , Label
                      "Discard cards with at least 2 {agility} icons"
                      [PayForAbility (abilityEffect attrs cost) []]
                  ]
              )
          else push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0)
    _ -> PassengerCar_167 <$> runMessage msg attrs
