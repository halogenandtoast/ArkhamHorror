module Arkham.Types.Location.Cards.PassengerCar_169
  ( passengerCar_169
  , PassengerCar_169(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (passengerCar_169)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Window

newtype PassengerCar_169 = PassengerCar_169 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passengerCar_169 :: LocationCard PassengerCar_169
passengerCar_169 = locationWith
  PassengerCar_169
  Cards.passengerCar_169
  2
  (PerPlayer 2)
  NoSymbol
  []
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasCount ClueCount env LocationId => HasModifiersFor env PassengerCar_169 where
  getModifiersFor _ target (PassengerCar_169 l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- unClueCount <$> getCount leftLocation
        pure $ toModifiers l [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasAbilities env PassengerCar_169 where
  getAbilities iid window (PassengerCar_169 attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env PassengerCar_169 where
  runMessage msg l@(PassengerCar_169 attrs@LocationAttrs {..}) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> do
      let cost = SkillIconCost 2 (singleton SkillWillpower)
      hasSkills <- getCanAffordCost iid (toSource attrs) Nothing [NonFast] cost
      l <$ if hasSkills
        then push
          (chooseOne
            iid
            [ Label
              "Take 2 horror"
              [InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2]
            , Label
              "Discard cards with at least 2 {willpower} icons"
              [ CreatePayAbilityCostEffect
                  (abilityEffect attrs cost)
                  (toSource attrs)
                  (toTarget attrs)
              ]
            ]
          )
        else push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2)
    _ -> PassengerCar_169 <$> runMessage msg attrs
