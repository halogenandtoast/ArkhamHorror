module Arkham.Act.Cards.TrialOfTheHuntress
  ( TrialOfTheHuntress(..)
  , trialOfTheHuntress
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message

newtype TrialOfTheHuntress = TrialOfTheHuntress ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

trialOfTheHuntress :: ActCard TrialOfTheHuntress
trialOfTheHuntress =
  act (1, E) TrialOfTheHuntress Cards.trialOfTheHuntress
    $ Just
    $ GroupClueCost (PerPlayer 2)
    $ LocationWithTitle "Rivertown"

instance RunMessage TrialOfTheHuntress where
  runMessage msg a@(TrialOfTheHuntress attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      blackCave <- genCard Locations.blackCave
      pushAll
        [ PlaceLocation blackCave
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> TrialOfTheHuntress <$> runMessage msg attrs
