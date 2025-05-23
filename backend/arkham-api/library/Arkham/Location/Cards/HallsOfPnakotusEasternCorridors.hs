module Arkham.Location.Cards.HallsOfPnakotusEasternCorridors (
  hallsOfPnakotusEasternCorridors,
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Movement
import Arkham.Prelude

newtype HallsOfPnakotusEasternCorridors = HallsOfPnakotusEasternCorridors LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallsOfPnakotusEasternCorridors
  :: LocationCard HallsOfPnakotusEasternCorridors
hallsOfPnakotusEasternCorridors =
  locationWith
    HallsOfPnakotusEasternCorridors
    Cards.hallsOfPnakotusEasternCorridors
    3
    (Static 1)
    (labelL .~ "hallsOfPnakotusEasternCorridors")

instance HasAbilities HallsOfPnakotusEasternCorridors where
  getAbilities (HallsOfPnakotusEasternCorridors attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 (Here <> DuringTurn You)
          $ FastAbility
          $ HandDiscardCost 1 #any
      ]

instance RunMessage HallsOfPnakotusEasternCorridors where
  runMessage msg l@(HallsOfPnakotusEasternCorridors attrs) = case msg of
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
          [ targetLabel lid [Move $ move (toSource attrs) iid lid]
          | lid <- otherHalls
          ]
      pure l
    _ -> HallsOfPnakotusEasternCorridors <$> runMessage msg attrs
