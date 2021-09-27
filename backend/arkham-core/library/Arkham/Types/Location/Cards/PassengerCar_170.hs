module Arkham.Types.Location.Cards.PassengerCar_170
  ( passengerCar_170
  , PassengerCar_170(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (passengerCar_170)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window

newtype PassengerCar_170 = PassengerCar_170 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passengerCar_170 :: LocationCard PassengerCar_170
passengerCar_170 = locationWith
  PassengerCar_170
  Cards.passengerCar_170
  3
  (PerPlayer 2)
  NoSymbol
  []
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasCount ClueCount env LocationId => HasModifiersFor env PassengerCar_170 where
  getModifiersFor _ target (PassengerCar_170 l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- unClueCount <$> getCount leftLocation
        pure $ toModifiers l [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities PassengerCar_170 where
  getAbilities (PassengerCar_170 x) = withBaseAbilities
    x
    [ mkAbility x 1
      $ ForcedAbility
      $ Enters Timing.After You
      $ LocationWithId
      $ toId x
    | locationRevealed x
    ]

instance LocationRunner env => RunMessage env PassengerCar_170 where
  runMessage msg l@(PassengerCar_170 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      let cost = SkillIconCost 2 (singleton SkillIntellect)
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
              "Take 2 horror"
              [InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2]
            , Label
              "Discard cards with at least 2 {intellect} icons"
              [ CreatePayAbilityCostEffect
                  (abilityEffect attrs cost)
                  (toSource attrs)
                  (toTarget attrs)
                  []
              ]
            ]
          )
        else push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2)
    _ -> PassengerCar_170 <$> runMessage msg attrs
