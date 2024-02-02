module Arkham.Location.Cards.PassengerCar_171 (
  passengerCar_171,
  PassengerCar_171 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (passengerCar_171)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype PassengerCar_171 = PassengerCar_171 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

passengerCar_171 :: LocationCard PassengerCar_171
passengerCar_171 =
  locationWith
    PassengerCar_171
    Cards.passengerCar_171
    1
    (PerPlayer 1)
    (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasModifiersFor PassengerCar_171 where
  getModifiersFor target (PassengerCar_171 l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
        Just leftLocation -> do
          clueCount <- field LocationClues leftLocation
          pure $ toModifiers l [Blocked | not locationRevealed && clueCount > 0]
        Nothing -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities PassengerCar_171 where
  getAbilities (PassengerCar_171 x) =
    withBaseAbilities
      x
      [ restrictedAbility x 1 Here
        $ ForcedAbility
        $ Enters Timing.After You
        $ LocationWithId
        $ toId x
      | locationRevealed x
      ]

instance RunMessage PassengerCar_171 where
  runMessage msg l@(PassengerCar_171 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let cost = SkillIconCost 1 (singleton WildIcon)
      hasSkills <- getCanAffordCost iid (toAbilitySource attrs 1) [] [mkWhen NonFast] cost
      player <- getPlayer iid
      if hasSkills
        then
          push
            . chooseOne player
            $ [ Label
                  "Take 1 damage and 1 horror"
                  [InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1]
              , Label
                  "Discard cards with at least 1 {wild} icons"
                  [PayForAbility (abilityEffect attrs cost) []]
              ]
        else push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1)
      pure l
    _ -> PassengerCar_171 <$> runMessage msg attrs
