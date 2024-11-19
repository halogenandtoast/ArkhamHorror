module Arkham.Agenda.Cards.ColdWelcome (ColdWelcome (..), coldWelcome) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet (EncounterSet (CreaturesInTheIce))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Helpers.Window (entering)
import Arkham.Matcher
import Arkham.Scenarios.IceAndDeath.Helpers

newtype ColdWelcome = ColdWelcome AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldWelcome :: AgendaCard ColdWelcome
coldWelcome = agenda (1, A) ColdWelcome Cards.coldWelcome (Static 4)

instance HasAbilities ColdWelcome where
  getAbilities (ColdWelcome a) = [placeSetAsideConnectedAbility a 1]

instance RunMessage ColdWelcome where
  runMessage msg a@(ColdWelcome attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      (spawning, rest) <- splitAt 1 <$> getSetAsideCardsMatching (cardIs Enemies.skitteringNonsense)
      lead <- getLead
      for_ spawning (`createEnemy_` lead)
      shuffleCardsIntoDeck Deck.EncounterDeck rest
      shuffleSetAsideIntoEncounterDeck CreaturesInTheIce
      advanceAgendaDeck attrs
      pure a
    UseCardAbility _iid (isSource attrs -> True) 1 (entering -> lid) _ -> do
      placeSetAsideConnectedLocations lid
      pure a
    _ -> ColdWelcome <$> liftRunMessage msg attrs
