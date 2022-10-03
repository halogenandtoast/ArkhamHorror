module Arkham.Location.Cards.PassengerCar_168
  ( passengerCar_168
  , PassengerCar_168(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( passengerCar_168 )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype PassengerCar_168 = PassengerCar_168 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passengerCar_168 :: LocationCard PassengerCar_168
passengerCar_168 = locationWith
  PassengerCar_168
  Cards.passengerCar_168
  4
  (PerPlayer 1)
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasModifiersFor PassengerCar_168 where
  getModifiersFor target (PassengerCar_168 l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- field LocationClues leftLocation
        pure $ toModifiers l [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities PassengerCar_168 where
  getAbilities (PassengerCar_168 x) = withBaseAbilities
    x
    [ restrictedAbility x 1 Here
      $ ForcedAbility
      $ Enters Timing.After You
      $ LocationWithId
      $ toId x
    | locationRevealed x
    ]

instance RunMessage PassengerCar_168 where
  runMessage msg l@(PassengerCar_168 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let cost = SkillIconCost 2 (singleton SkillCombat)
      hasSkills <- getCanAffordCost
        iid
        (toSource attrs)
        Nothing
        [Window Timing.When NonFast]
        cost
      l <$ if hasSkills
        then push
          (chooseOne
            iid
            [ Label
              "Take 2 damage"
              [InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0]
            , Label
              "Discard cards with at least 2 {combat} icons"
              [PayForAbility (abilityEffect attrs cost) []]
            ]
          )
        else push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0)
    _ -> PassengerCar_168 <$> runMessage msg attrs
