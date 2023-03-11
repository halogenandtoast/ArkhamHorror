module Arkham.Location.Cards.HallsOfPnakotusEasternCorridors
  ( hallsOfPnakotusEasternCorridors
  , HallsOfPnakotusEasternCorridors(..)
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
import Arkham.Movement

newtype HallsOfPnakotusEasternCorridors = HallsOfPnakotusEasternCorridors LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallsOfPnakotusEasternCorridors
  :: LocationCard HallsOfPnakotusEasternCorridors
hallsOfPnakotusEasternCorridors = locationWith
  HallsOfPnakotusEasternCorridors
  Cards.hallsOfPnakotusEasternCorridors
  3
  (Static 1)
  (labelL .~ "hallsOfPnakotusEasternCorridors")

instance HasAbilities HallsOfPnakotusEasternCorridors where
  getAbilities (HallsOfPnakotusEasternCorridors attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 (Here <> DuringTurn You)
      $ FastAbility
      $ HandDiscardCost 1 AnyCard
    ]

instance RunMessage HallsOfPnakotusEasternCorridors where
  runMessage msg l@(HallsOfPnakotusEasternCorridors attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      otherHalls <-
        selectList $ LocationWithTitle "Halls of Pnakotus" <> NotLocation
          (LocationWithId $ toId attrs)
      push $ chooseOne
        iid
        [ targetLabel lid [MoveTo $ move (toSource attrs) iid lid]
        | lid <- otherHalls
        ]
      pure l
    _ -> HallsOfPnakotusEasternCorridors <$> runMessage msg attrs
