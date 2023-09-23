module Arkham.Act.Cards.HiddenAgendas (
  HiddenAgendas (..),
  hiddenAgendas,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message
import Arkham.Movement
import Arkham.Projection
import Arkham.Trait (Trait (Monster))

newtype HiddenAgendas = HiddenAgendas ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hiddenAgendas :: ActCard HiddenAgendas
hiddenAgendas = act (1, A) HiddenAgendas Cards.hiddenAgendas Nothing

instance RunMessage HiddenAgendas where
  runMessage msg a@(HiddenAgendas attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      entryHall <- selectJust $ locationIs Locations.entryHallAtDeathsDoorstep
      victorianHalls <- selectJust $ locationIs Locations.victorianHalls
      balcony <- selectJust $ locationIs Locations.balconyAtDeathsDoorstep
      office <- selectJust $ locationIs Locations.office
      billiardsRoom <- selectJust $ locationIs Locations.billiardsRoom
      masterBedroom <- selectJust $ locationIs Locations.masterBedroom
      trophyRoom <- selectJust $ locationIs Locations.trophyRoom

      enemyIds <- selectList $ enemyAt entryHall
      investigatorIds <- selectList $ investigatorAt entryHall

      entryHallSpectral <- getSetAsideCard Locations.entryHallSpectral
      victorianHallsSpectral <- getSetAsideCard Locations.victorianHallsSpectral
      balconySpectral <- getSetAsideCard Locations.balconySpectral
      officeSpectral <- getSetAsideCard Locations.officeSpectral
      billiardsRoomSpectral <- getSetAsideCard Locations.billiardsRoomSpectral
      masterBedroomSpectral <- getSetAsideCard Locations.masterBedroomSpectral
      trophyRoomSpectral <- getSetAsideCard Locations.trophyRoomSpectral

      theSpectralWatcher <- getSetAsideCard Enemies.theSpectralWatcher

      theWatcherSet <-
        filter (/= theSpectralWatcher)
          <$> getSetAsideEncounterSet EncounterSet.TheWatcher
      realmOfDeathSet <- getSetAsideEncounterSet EncounterSet.RealmOfDeath

      pushAll
        $ map
          (\iid -> Move $ move (toSource attrs) iid victorianHalls)
          investigatorIds
        <> map
          (\eid -> Move $ move (toSource attrs) eid victorianHalls)
          enemyIds
        <> [ ReplaceLocation entryHall entryHallSpectral DefaultReplace
           , ReplaceLocation victorianHalls victorianHallsSpectral DefaultReplace
           , ReplaceLocation balcony balconySpectral DefaultReplace
           , ReplaceLocation office officeSpectral DefaultReplace
           , ReplaceLocation billiardsRoom billiardsRoomSpectral DefaultReplace
           , ReplaceLocation masterBedroom masterBedroomSpectral DefaultReplace
           , ReplaceLocation trophyRoom trophyRoomSpectral DefaultReplace
           , NextAdvanceActStep (toId a) 0
           , NextAdvanceActStep (toId a) 1
           , SpawnEnemyAt theSpectralWatcher entryHall
           , ShuffleCardsIntoDeck
              Deck.EncounterDeck
              (theWatcherSet <> realmOfDeathSet)
           , ShuffleEncounterDiscardBackIn
           , advanceActDeck attrs
           ]

      pure a
    NextAdvanceActStep aid 0 | aid == toId a -> do
      locations <- selectList $ LocationWithInvestigator Anyone
      pushAll $ map (RevealLocation Nothing) locations
      pure a
    NextAdvanceActStep aid n | aid == toId a -> do
      monsters <- getSetAsideCardsMatching $ CardWithTrait Monster
      unless (null monsters) $ do
        iids <- getInvestigatorIds
        let iid = fromJustNote "error" $ iids !!? (n - 1)
        mLocation <- field InvestigatorLocation iid
        for_ mLocation $ \lid -> do
          pushAll
            [ FocusCards monsters
            , chooseOne
                iid
                [ targetLabel (toCardId monster) [SpawnEnemyAt monster lid]
                | monster <- monsters
                ]
            , UnfocusCards
            , NextAdvanceActStep aid ((n `mod` length iids) + 1)
            ]

      pure a
    _ -> HiddenAgendas <$> runMessage msg attrs
