module Arkham.Location.Cards.HallsOfPnakotusNorthernCorridors
  ( hallsOfPnakotusNorthernCorridors
  , HallsOfPnakotusNorthernCorridors(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding ( DuringTurn )
import Arkham.Message

newtype HallsOfPnakotusNorthernCorridors = HallsOfPnakotusNorthernCorridors LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallsOfPnakotusNorthernCorridors
  :: LocationCard HallsOfPnakotusNorthernCorridors
hallsOfPnakotusNorthernCorridors = locationWith
  HallsOfPnakotusNorthernCorridors
  Cards.hallsOfPnakotusNorthernCorridors
  3
  (Static 1)
  (labelL .~ "hallsOfPnakotusNorthernCorridors")

instance HasAbilities HallsOfPnakotusNorthernCorridors where
  getAbilities (HallsOfPnakotusNorthernCorridors attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 (Here <> DuringTurn You)
      $ FastAbility
      $ HandDiscardCost 1 AnyCard
    ]

instance RunMessage HallsOfPnakotusNorthernCorridors where
  runMessage msg l@(HallsOfPnakotusNorthernCorridors attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      otherHalls <-
        selectList $ LocationWithTitle "Halls of Pnakotus" <> NotLocation
          (LocationWithId $ toId attrs)
      push $ chooseOne
        iid
        [ targetLabel lid [MoveTo (toSource attrs) iid lid]
        | lid <- otherHalls
        ]
      pure l
    _ -> HallsOfPnakotusNorthernCorridors <$> runMessage msg attrs
