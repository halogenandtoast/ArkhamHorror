module Arkham.Types.Location.Cards.PassengerCar_168
  ( passengerCar_168
  , PassengerCar_168(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (passengerCar_168)
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

newtype PassengerCar_168 = PassengerCar_168 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passengerCar_168 :: LocationCard PassengerCar_168
passengerCar_168 = locationWith
  PassengerCar_168
  Cards.passengerCar_168
  4
  (PerPlayer 1)
  NoSymbol
  []
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasCount ClueCount env LocationId => HasModifiersFor env PassengerCar_168 where
  getModifiersFor _ target (PassengerCar_168 l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- unClueCount <$> getCount leftLocation
        pure $ toModifiers l [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasAbilities env PassengerCar_168 where
  getAbilities iid window (PassengerCar_168 attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env PassengerCar_168 where
  runMessage msg l@(PassengerCar_168 attrs@LocationAttrs {..}) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> do
      let cost = SkillIconCost 2 (singleton SkillCombat)
      hasSkills <- getCanAffordCost iid (toSource attrs) Nothing [NonFast] cost
      l <$ if hasSkills
        then push
          (chooseOne
            iid
            [ Label
              "Take 2 damage"
              [InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0]
            , Label
              "Discard cards with at least 2 {combat} icons"
              [ CreatePayAbilityCostEffect
                  (abilityEffect attrs cost)
                  (toSource attrs)
                  (toTarget attrs)
              ]
            ]
          )
        else push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0)
    _ -> PassengerCar_168 <$> runMessage msg attrs
