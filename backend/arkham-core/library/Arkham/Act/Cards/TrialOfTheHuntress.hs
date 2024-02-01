module Arkham.Act.Cards.TrialOfTheHuntress (
  TrialOfTheHuntress (..),
  trialOfTheHuntress,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype TrialOfTheHuntress = TrialOfTheHuntress ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

trialOfTheHuntress :: ActCard TrialOfTheHuntress
trialOfTheHuntress =
  act (1, E) TrialOfTheHuntress Cards.trialOfTheHuntress
    $ Just
    $ GroupClueCost (PerPlayer 2)
    $ locationIs Locations.rivertown

instance RunMessage TrialOfTheHuntress where
  runMessage msg a@(TrialOfTheHuntress attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide F attrs -> do
      placeBlackCave <- placeLocationCard_ Locations.blackCave
      pushAll
        [ placeBlackCave
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> TrialOfTheHuntress <$> runMessage msg attrs
