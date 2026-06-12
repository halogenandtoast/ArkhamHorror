module Arkham.Agenda.Cards.TheNewGirl (theNewGirl) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation (createExhausted)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers (shuffleIntoLeadsDeck)
import Arkham.Trait (Trait (Hideout, Monster, Suspect))

newtype TheNewGirl = TheNewGirl AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNewGirl :: AgendaCard TheNewGirl
theNewGirl = agenda (1, A) TheNewGirl Cards.theNewGirl (Static 3)

instance HasAbilities TheNewGirl where
  getAbilities (TheNewGirl x) =
    [mkAbility x 1 $ forced $ AgendaWouldAdvance #when #doom (be x) | onSide A x]

instance RunMessage TheNewGirl where
  runMessage msg a@(TheNewGirl attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ RemoveAllDoomFromPlay defaultRemoveDoomMatchers
      monsters <- getSetAsideCardsMatching (CardWithTrait Monster)
      for_ (nonEmpty monsters) \ms -> do
        monster <- sample ms
        shuffleIntoLeadsDeck [monster]
      when (length monsters <= 1) $ advanceAgenda attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      innsmouthSquare <- selectJust $ location_ "Innsmouth Square"
      createSetAsideEnemyWith_ Enemies.angryMob innsmouthSquare createExhausted
      n <- perPlayer 1
      selectEach (EnemyWithTrait Suspect) \enemy -> placeClues attrs enemy n
      selectEach (LocationWithTrait Hideout) \hideout -> placeClues attrs hideout n
      advanceAgendaDeck attrs
      pure a
    _ -> TheNewGirl <$> liftRunMessage msg attrs
