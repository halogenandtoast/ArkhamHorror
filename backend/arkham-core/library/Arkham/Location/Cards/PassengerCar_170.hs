module Arkham.Location.Cards.PassengerCar_170 (
  passengerCar_170,
  PassengerCar_170 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (passengerCar_170)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype PassengerCar_170 = PassengerCar_170 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

passengerCar_170 :: LocationCard PassengerCar_170
passengerCar_170 =
  locationWith
    PassengerCar_170
    Cards.passengerCar_170
    3
    (PerPlayer 2)
    (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasModifiersFor PassengerCar_170 where
  getModifiersFor target (PassengerCar_170 l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
        Just leftLocation -> do
          clueCount <- field LocationClues leftLocation
          pure $ toModifiers l [Blocked | not locationRevealed && clueCount > 0]
        Nothing -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities PassengerCar_170 where
  getAbilities (PassengerCar_170 x) =
    withBaseAbilities
      x
      [ restrictedAbility x 1 Here
        $ ForcedAbility
        $ Enters Timing.After You
        $ LocationWithId
        $ toId x
      | locationRevealed x
      ]

instance RunMessage PassengerCar_170 where
  runMessage msg l@(PassengerCar_170 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let cost = SkillIconCost 2 (singleton $ SkillIcon SkillIntellect)
      hasSkills <- getCanAffordCost iid (toAbilitySource attrs 1) [] [mkWhen NonFast] cost
      player <- getPlayer iid
      if hasSkills
        then
          push
            . chooseOne player
            $ [ Label
                  "Take 2 horror"
                  [InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2]
              , Label
                  "Discard cards with at least 2 {intellect} icons"
                  [PayForAbility (abilityEffect attrs cost) []]
              ]
        else push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2)
      pure l
    _ -> PassengerCar_170 <$> runMessage msg attrs
