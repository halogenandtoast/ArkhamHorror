module Arkham.Act.Cards.SearchForTheGate (searchForTheGate) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers (exploreAction_, runExplore)
import Arkham.Helpers.Query (getSetAsideCardMaybe)
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Deck

newtype SearchForTheGate = SearchForTheGate ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheGate :: ActCard SearchForTheGate
searchForTheGate = act (1, A) SearchForTheGate Cards.searchForTheGate (groupClueCost $ PerPlayer 2)

instance HasAbilities SearchForTheGate where
  getAbilities (SearchForTheGate a) = withBaseAbilities a [mkAbility a 1 exploreAction_]

instance RunMessage SearchForTheGate where
  runMessage msg a@(SearchForTheGate attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      runExplore iid (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      whenJustM (getSetAsideCardMaybe Locations.eldritchGate) \eldritchGate ->
        shuffleCardsIntoDeck ExplorationDeck (only eldritchGate)
      advanceActDeck attrs
      pure a
    _ -> SearchForTheGate <$> liftRunMessage msg attrs
