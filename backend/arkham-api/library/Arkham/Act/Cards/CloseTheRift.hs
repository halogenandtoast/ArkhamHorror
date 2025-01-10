module Arkham.Act.Cards.CloseTheRift (closeTheRift) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Location.Cards qualified as Locations
import Arkham.Helpers.Query (getJustLocationByName)
import Arkham.Message.Lifted.Choose

newtype CloseTheRift = CloseTheRift ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeTheRift :: ActCard CloseTheRift
closeTheRift =
  act
    (3, A)
    CloseTheRift
    Cards.closeTheRift
    (Just $ GroupClueCost (PerPlayer 3) "The Edge of the Universe")

instance HasAbilities CloseTheRift where
  getAbilities (CloseTheRift x) = extend1 x $ mkAbility x 1 actionAbility

instance RunMessage CloseTheRift where
  runMessage msg a@(CloseTheRift attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DiscardTopOfEncounterDeck iid 3 (toSource attrs) (Just $ toTarget attrs)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      removeLocation =<< getJustLocationByName "The Edge of the Universe"
      placeLocation_ Locations.tearThroughTime
      advanceActDeck attrs
      pure a
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      let locationCards = filterLocations cards
      focusCards locationCards do
        chooseTargetM iid locationCards (drawCard iid)
      pure a
    _ -> CloseTheRift <$> liftRunMessage msg attrs
