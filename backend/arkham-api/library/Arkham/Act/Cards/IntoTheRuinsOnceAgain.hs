module Arkham.Act.Cards.IntoTheRuinsOnceAgain (intoTheRuinsOnceAgain) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Deck

newtype IntoTheRuinsOnceAgain = IntoTheRuinsOnceAgain ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheRuinsOnceAgain :: ActCard IntoTheRuinsOnceAgain
intoTheRuinsOnceAgain =
  act (1, A) IntoTheRuinsOnceAgain Cards.intoTheRuinsOnceAgain (groupClueCost $ PerPlayer 3)

instance HasAbilities IntoTheRuinsOnceAgain where
  getAbilities (IntoTheRuinsOnceAgain a) = extend1 a $ mkAbility a 1 exploreAction_

instance RunMessage IntoTheRuinsOnceAgain where
  runMessage msg a@(IntoTheRuinsOnceAgain attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      runExplore iid (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      chamberOfTime <- getSetAsideCard Locations.chamberOfTime
      shuffleCardsIntoDeck ExplorationDeck [chamberOfTime]
      addToVictory (toTarget attrs)
      advanceActDeck attrs
      pure a
    _ -> IntoTheRuinsOnceAgain <$> liftRunMessage msg attrs
