module Arkham.Act.Cards.TheCircleUndone.UnionAndDisillusion.FatedSouls (fatedSouls) where

import Arkham.Act.CardDefs.TheCircleUndone.UnionAndDisillusion qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Deck
import Arkham.Enemy.CardDefs.TheCircleUndone.TheWatcher qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getJustLocationByName, getSetAsideCardsMatching)
import Arkham.Location.CardDefs.TheCircleUndone.UnionAndDisillusion qualified as Locations
import Arkham.Matcher hiding (EncounterDeck)
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.TheCircleUndone.UnionAndDisillusion.Helpers
import Arkham.Treachery.CardDefs.TheCircleUndone.TheWatcher qualified as Treacheries
import Arkham.Treachery.CardDefs.TheCircleUndone.UnionAndDisillusion qualified as Treacheries

newtype FatedSouls = FatedSouls ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor FatedSouls where
  getModifiersFor (FatedSouls attrs) = modifySelect attrs Anyone [CannotMove, CannotBeMoved]

fatedSouls :: ActCard FatedSouls
fatedSouls = act (2, A) FatedSouls Cards.fatedSouls (groupClueCost (PerPlayer 2))

instance RunMessage FatedSouls where
  runMessage msg a@(FatedSouls attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectForMaybeM (OutOfPlayEnemy SetAsideZone $ enemyIs Enemies.theSpectralWatcher) \watcher -> do
        place watcher =<< getJustLocationByName "Miskatonic River"

      watchersGrasp <- getSetAsideCardsMatching $ cardIs Treacheries.watchersGrasp
      watchersGaze <- getSetAsideCardsMatching $ cardIs Treacheries.watchersGaze
      shuffleCardsIntoDeck EncounterDeck (watchersGrasp <> watchersGaze)

      shuffleEncounterDiscardBackIn
      advanceActDeck attrs

      locations <- select $ LocationIsInFrontOf Anyone
      pushAll $ map PutLocationInCenter locations

      sidedWithTheCoven <- getHasRecord TheInvestigatorsSidedWithTheCoven
      if sidedWithTheCoven
        then lightBrazier =<< placeSetAsideLocation Locations.theGeistTrap
        else placeSetAsideLocation_ Locations.theGeistTrap
      pure a
    _ -> FatedSouls <$> liftRunMessage msg attrs
