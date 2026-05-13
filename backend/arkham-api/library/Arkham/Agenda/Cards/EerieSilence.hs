module Arkham.Agenda.Cards.EerieSilence (eerieSilence) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.Message.Lifted.Story
import Arkham.Placement
import Arkham.Story.Cards qualified as Stories

newtype EerieSilence = EerieSilence AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eerieSilence :: AgendaCard EerieSilence
eerieSilence = agenda (1, A) EerieSilence Cards.eerieSilence (Static 2)

instance RunMessage EerieSilence where
  runMessage msg a@(EerieSilence attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      -- "Find the set-aside The Predatory House story card and resolve its text."
      predatoryHouses <- getSetAsideCardsMatching (cardIs Stories.thePredatoryHouse)
      for_ (nonEmpty predatoryHouses) \(card :| _) ->
        resolveStoryWithPlacement lead card Global
      advanceAgendaDeck attrs
      pure a
    _ -> EerieSilence <$> liftRunMessage msg attrs
