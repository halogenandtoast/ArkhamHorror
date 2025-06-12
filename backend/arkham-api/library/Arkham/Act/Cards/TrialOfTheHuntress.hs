module Arkham.Act.Cards.TrialOfTheHuntress (trialOfTheHuntress) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype TrialOfTheHuntress = TrialOfTheHuntress ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

trialOfTheHuntress :: ActCard TrialOfTheHuntress
trialOfTheHuntress =
  act (1, E) TrialOfTheHuntress Cards.trialOfTheHuntress
    $ Just
    $ GroupClueCost (PerPlayer 2)
    $ locationIs Locations.rivertown

instance RunMessage TrialOfTheHuntress where
  runMessage msg a@(TrialOfTheHuntress attrs) = runQueueT $ case msg of
    AdvanceAct (isSide F attrs -> True) _ _ -> do
      placeLocation_ =<< genCard Locations.blackCave
      advanceActDeck attrs
      pure a
    _ -> TrialOfTheHuntress <$> liftRunMessage msg attrs
