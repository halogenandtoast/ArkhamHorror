module Arkham.Location.Cards.HallsOfPnakotusWesternCorridors (
  hallsOfPnakotusWesternCorridors,
  HallsOfPnakotusWesternCorridors (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Movement

newtype HallsOfPnakotusWesternCorridors = HallsOfPnakotusWesternCorridors LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallsOfPnakotusWesternCorridors
  :: LocationCard HallsOfPnakotusWesternCorridors
hallsOfPnakotusWesternCorridors =
  locationWith
    HallsOfPnakotusWesternCorridors
    Cards.hallsOfPnakotusWesternCorridors
    3
    (Static 1)
    (labelL .~ "hallsOfPnakotusWesternCorridors")

instance HasAbilities HallsOfPnakotusWesternCorridors where
  getAbilities (HallsOfPnakotusWesternCorridors attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 (Here <> DuringTurn You)
          $ FastAbility
          $ HandDiscardCost 1 #any
      ]

instance RunMessage HallsOfPnakotusWesternCorridors where
  runMessage msg l@(HallsOfPnakotusWesternCorridors attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      otherHalls <-
        select
          $ LocationWithTitle "Halls of Pnakotus"
          <> NotLocation
            (LocationWithId $ toId attrs)
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel lid [MoveTo $ move (toSource attrs) iid lid]
          | lid <- otherHalls
          ]
      pure l
    _ -> HallsOfPnakotusWesternCorridors <$> runMessage msg attrs
