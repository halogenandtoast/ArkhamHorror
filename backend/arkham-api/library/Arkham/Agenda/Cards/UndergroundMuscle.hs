module Arkham.Agenda.Cards.UndergroundMuscle (undergroundMuscle) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype UndergroundMuscle = UndergroundMuscle AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undergroundMuscle :: AgendaCard UndergroundMuscle
undergroundMuscle = agenda (2, A) UndergroundMuscle Cards.undergroundMuscle (Static 3)

instance RunMessage UndergroundMuscle where
  runMessage msg a@(UndergroundMuscle attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      (enemies, rest) <- splitAt 1 <$> (shuffleM =<< getSetAsideEncounterSet HideousAbominations)
      cloverClubLounge <- getJustLocationByName "Clover Club Lounge"
      for_ enemies (`createEnemyAt_` cloverClubLounge)
      shuffleEncounterDiscardBackIn

      strikingFear <- concatMapM getSetAsideEncounterSet [StrikingFear, ErraticFear]
      shuffleCardsIntoDeck Deck.EncounterDeck (rest <> strikingFear)

      laBellaLuna <- getJustLocationByName "La Bella Luna"
      selectEach (InvestigatorAt $ LocationWithId laBellaLuna) \iid -> moveTo attrs iid cloverClubLounge
      selectEach (at_ (LocationWithId laBellaLuna) <> UnengagedEnemy) (`enemyMoveTo` cloverClubLounge)
      removeLocation laBellaLuna
      advanceAgendaDeck attrs
      pure a
    _ -> UndergroundMuscle <$> liftRunMessage msg attrs
