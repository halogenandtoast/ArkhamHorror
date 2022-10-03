module Arkham.Location.Cards.DiningCar
  ( diningCar
  , DiningCar(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( diningCar )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype DiningCar = DiningCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diningCar :: LocationCard DiningCar
diningCar = locationWith
  DiningCar
  Cards.diningCar
  2
  (Static 0)
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasModifiersFor DiningCar where
  getModifiersFor target (DiningCar l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- field LocationClues leftLocation
        pure $ toModifiers l [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities DiningCar where
  getAbilities (DiningCar x) =
    withBaseAbilities x
      $ [ restrictedAbility x 1 Here
          $ ForcedAbility
          $ RevealLocation Timing.After You
          $ LocationWithId
          $ toId x
        | locationRevealed x
        ]

instance RunMessage DiningCar where
  runMessage msg l@(DiningCar attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ push
        (FindAndDrawEncounterCard iid (CardWithTitle "Grappling Horror"))
    _ -> DiningCar <$> runMessage msg attrs
