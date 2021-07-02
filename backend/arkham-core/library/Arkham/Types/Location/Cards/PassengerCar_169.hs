module Arkham.Types.Location.Cards.PassengerCar_169
  ( passengerCar_169
  , PassengerCar_169(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (passengerCar_169)
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType

newtype PassengerCar_169 = PassengerCar_169 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passengerCar_169 :: LocationId -> PassengerCar_169
passengerCar_169 =
  PassengerCar_169 . (connectsToL .~ setFromList [LeftOf, RightOf]) . baseAttrs
    Cards.passengerCar_169
    2
    (PerPlayer 2)
    NoSymbol
    []

instance HasCount ClueCount env LocationId => HasModifiersFor env PassengerCar_169 where
  getModifiersFor _ target (PassengerCar_169 location@LocationAttrs {..})
    | isTarget location target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- unClueCount <$> getCount leftLocation
        pure $ toModifiers
          location
          [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env PassengerCar_169 where
  getActions iid window (PassengerCar_169 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env PassengerCar_169 where
  runMessage msg l@(PassengerCar_169 attrs@LocationAttrs {..}) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> do
      let cost = SkillIconCost 2 (singleton SkillWillpower)
      hasSkills <- getCanAffordCost iid (toSource attrs) Nothing cost
      l <$ if hasSkills
        then unshiftMessage
          (chooseOne
            iid
            [ Label
              "Take 2 horror"
              [InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2]
            , Label
              "Discard cards with at least 2 {willpower} icons"
              [ CreatePayAbilityCostEffect
                Nothing
                (toSource attrs)
                (toTarget attrs)
              , PayAbilityCost (toSource attrs) iid Nothing cost
              , PayAbilityCostFinished (toSource attrs) iid
              ]
            ]
          )
        else unshiftMessage
          (InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2)
    _ -> PassengerCar_169 <$> runMessage msg attrs
