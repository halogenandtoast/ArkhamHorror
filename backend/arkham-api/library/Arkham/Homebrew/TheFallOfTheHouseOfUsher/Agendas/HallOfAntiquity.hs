module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Agendas.HallOfAntiquity (hallOfAntiquity) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Agendas qualified as Cards
import Arkham.Matcher

newtype HallOfAntiquity = HallOfAntiquity AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfAntiquity :: AgendaCard HallOfAntiquity
hallOfAntiquity = agenda (1, A) HallOfAntiquity Cards.hallOfAntiquity (Static 3)

instance RunMessage HallOfAntiquity where
  runMessage msg a@(HallOfAntiquity attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- Either face of the room answers to its printed name.
      studio <- selectJust $ LocationWithTitle "Roderick's Studio"
      placeTokens attrs studio #damage 4
      advanceAgendaDeck attrs
      pure a
    _ -> HallOfAntiquity <$> liftRunMessage msg attrs
