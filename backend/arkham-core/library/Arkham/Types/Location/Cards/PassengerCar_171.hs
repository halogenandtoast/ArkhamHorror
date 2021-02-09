module Arkham.Types.Location.Cards.PassengerCar_171
  ( passengerCar_171
  , PassengerCar_171(..)
  )
where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype PassengerCar_171 = PassengerCar_171 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passengerCar_171 :: PassengerCar_171
passengerCar_171 = PassengerCar_171
  $ base { locationConnectsTo = setFromList [LeftOf, RightOf] }
 where
  base = baseAttrs
    "02171"
    (Name "Passenger Car" Nothing)
    EncounterSet.TheEssexCountyExpress
    1
    (PerPlayer 1)
    NoSymbol
    []
    (singleton Train)

instance HasCount ClueCount env LocationId => HasModifiersFor env PassengerCar_171 where
  getModifiersFor _ target (PassengerCar_171 location@LocationAttrs {..})
    | isTarget location target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- unClueCount <$> getCount leftLocation
        pure $ toModifiers
          location
          [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env PassengerCar_171 where
  getActions iid window (PassengerCar_171 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env PassengerCar_171 where
  runMessage msg l@(PassengerCar_171 attrs@LocationAttrs {..}) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> do
      let cost = SkillIconCost 1 (singleton SkillWild)
      hasSkills <- getCanAffordCost iid (toSource attrs) Nothing cost
      l <$ if hasSkills
        then unshiftMessage
          (chooseOne
            iid
            [ Label
              "Take 1 damage and 1 horror"
              [InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1]
            , Label
              "Discard cards with at least 1 {wild} icons"
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
          (InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1)
    _ -> PassengerCar_171 <$> runMessage msg attrs
