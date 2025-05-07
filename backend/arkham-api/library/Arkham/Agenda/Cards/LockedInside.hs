module Arkham.Agenda.Cards.LockedInside (lockedInside) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Choose
import Arkham.Helpers.Query
import Arkham.Scenario.Deck

newtype LockedInside = LockedInside AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedInside :: AgendaCard LockedInside
lockedInside = agenda (1, A) LockedInside Cards.lockedInside (Static 2)

instance RunMessage LockedInside where
  runMessage msg a@(LockedInside attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      shuffleScenarioDeckIntoEncounterDeck LunaticsDeck
      shuffleEncounterDiscardBackIn
      randomlyChooseFrom attrs lead MonstersDeck 1
      advanceAgendaDeck attrs
      pure a
    ChoseCards _ chose | isTarget attrs chose.target -> do
      placeUnderneath ActDeckTarget chose.cards
      pure a
    _ -> LockedInside <$> liftRunMessage msg attrs
