module Arkham.Act.Cards.FatedSouls (fatedSouls) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getJustLocationByName, getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (EncounterDeck)
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.UnionAndDisillusion.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

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

      whenHasRecord TheInvestigatorsSidedWithTheCoven do
        lightBrazier =<< placeSetAsideLocation Locations.theGeistTrap
      pure a
    _ -> FatedSouls <$> liftRunMessage msg attrs
