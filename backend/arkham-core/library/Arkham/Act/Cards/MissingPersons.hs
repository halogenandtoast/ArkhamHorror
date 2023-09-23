module Arkham.Act.Cards.MissingPersons (
  MissingPersons (..),
  missingPersons,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message

newtype MissingPersons = MissingPersons ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

missingPersons :: ActCard MissingPersons
missingPersons =
  act (1, C) MissingPersons Cards.missingPersons
    $ Just
    $ GroupClueCost (PerPlayer 1)
    $ LocationWithTitle "Easttown"

instance RunMessage MissingPersons where
  runMessage msg a@(MissingPersons attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide D attrs -> do
      arkhamPoliceStation <- genCard Locations.arkhamPoliceStation
      placeArkhamPoliceStation <- placeLocation_ arkhamPoliceStation
      pushAll
        [ placeArkhamPoliceStation
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> MissingPersons <$> runMessage msg attrs
