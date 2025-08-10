module Arkham.Act.Cards.HiddenAgendas (hiddenAgendas) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (replaceLocation, withLocationOf)
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Monster))

newtype HiddenAgendas = HiddenAgendas ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hiddenAgendas :: ActCard HiddenAgendas
hiddenAgendas = act (1, A) HiddenAgendas Cards.hiddenAgendas Nothing

instance RunMessage HiddenAgendas where
  runMessage msg a@(HiddenAgendas attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      entryHall <- selectJust $ locationIs Locations.entryHallAtDeathsDoorstep
      enemyIds <- select $ enemyAt entryHall
      investigatorIds <- select $ investigatorAt entryHall

      victorianHalls <- selectJust $ locationIs Locations.victorianHalls
      for_ investigatorIds \iid -> moveTo attrs iid victorianHalls
      for_ enemyIds \eid -> enemyMoveTo attrs eid victorianHalls

      replaceLocation entryHall =<< getSetAsideCard Locations.entryHallSpectral
      replaceLocation victorianHalls =<< getSetAsideCard Locations.victorianHallsSpectral

      balcony <- selectJust $ locationIs Locations.balconyAtDeathsDoorstep
      replaceLocation balcony =<< getSetAsideCard Locations.balconySpectral

      office <- selectJust $ locationIs Locations.office
      replaceLocation office =<< getSetAsideCard Locations.officeSpectral

      billiardsRoom <- selectJust $ locationIs Locations.billiardsRoom
      replaceLocation billiardsRoom =<< getSetAsideCard Locations.billiardsRoomSpectral

      masterBedroom <- selectJust $ locationIs Locations.masterBedroom
      replaceLocation masterBedroom =<< getSetAsideCard Locations.masterBedroomSpectral

      trophyRoom <- selectJust $ locationIs Locations.trophyRoom
      replaceLocation trophyRoom =<< getSetAsideCard Locations.trophyRoomSpectral

      doStep 0 msg
      eachInvestigator (`forInvestigator` msg)

      theSpectralWatcher <- getSetAsideCard Enemies.theSpectralWatcher
      spawnEnemyAt_ theSpectralWatcher entryHall

      theWatcherSet <-
        filter (/= theSpectralWatcher)
          <$> getSetAsideEncounterSet EncounterSet.TheWatcher
      realmOfDeathSet <- getSetAsideEncounterSet EncounterSet.RealmOfDeath

      shuffleCardsIntoDeck Deck.EncounterDeck (theWatcherSet <> realmOfDeathSet)
      shuffleEncounterDiscardBackIn
      advanceActDeck attrs

      pure a
    DoStep 0 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      selectEach (LocationWithInvestigator Anyone) reveal
      pure a
    ForInvestigator iid (AdvanceAct (isSide B attrs -> True) _ _) -> do
      monsters <- getSetAsideCardsMatching $ CardWithTrait Monster
      unless (null monsters) $ do
        withLocationOf iid \lid -> do
          focusCards monsters $ chooseTargetM iid monsters (`spawnEnemyAt_` lid)
      pure a
    _ -> HiddenAgendas <$> liftRunMessage msg attrs
